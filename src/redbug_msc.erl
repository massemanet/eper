%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 Mar 2010 by Mats Cronqvist <masse@kreditor.se>

%% msc - match spec compiler
%% transforms a string to a call trace expression;
%% {MFA,MatchSpec} == {{M,F,A},{Head,Cond,Body}}

-module('redbug_msc').
-author('Mats Cronqvist').

-export([transform/1]).
-export([unit/0]).

-define(is_string(Str), (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

transform(Es = [_|_]) when not ?is_string(Es) ->
  [compile(parse(to_string(E))) || E <- Es];
transform(E) ->
  transform([E]).

to_string(A) when is_atom(A)    -> atom_to_list(A);
to_string(S) when ?is_string(S) -> S;
to_string(X)                    -> exit({illegal_input,X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compiler
%% returns {{Module,Function,Arity},{Head,Cond,Body}}
compile({M,F,'_',[],Actions}) ->
  {{M,F,'_'},{[],[],compile_acts(Actions)}};
compile({M,F,Ari,[],Actions}) when is_integer(Ari) ->
  {{M,F,Ari},{[],[],compile_acts(Actions)}};
compile({M,F,As,Gs,Actions}) when is_list(As) ->
  {Vars,Args} = compile_args(As),
  {{M,F,length(As)},{Args,compile_guards(Gs,Vars),compile_acts(Actions)}}.

compile_acts(As) ->
  [ac_fun(A)|| A <- As].

ac_fun("stack") -> {message,{process_dump}};
ac_fun("return")-> {return_trace};
ac_fun(X)       -> exit({unknown_action,X}).

compile_guards(Gs,Vars) ->
  {Vars,O} = lists:foldr(fun gd_fun/2,{Vars,[]},Gs),
  O.

gd_fun({Op,As},{Vars,O}) when is_list(As) -> % function
  {Vars,[unpack_op(Op,As,Vars)|O]};
gd_fun({Op,V},{Vars,O}) ->                   % unary
  {Vars,[{Op,unpack_var(V,Vars)}|O]};
gd_fun({Op,V1,V2},{Vars,O}) ->               % binary
  {Vars,[{Op,unpack_var(V1,Vars),unpack_var(V2,Vars)}|O]}.

unpack_op(Op,As,Vars) ->
  list_to_tuple([Op|[unpack_var(A,Vars)||A<-As]]).

unpack_var({var,Var},Vars) ->
  case proplists:get_value(Var,Vars) of
    undefined -> exit({unbound_variable,Var});
    V -> V
  end;
unpack_var({Op,As},Vars) when is_list(As) ->
  unpack_op(Op,As,Vars);
unpack_var({Type,Val},_) ->
  assert_type(Type,Val),
  Val.

compile_args(As) ->
  lists:foldl(fun ca_fun/2,{[],[]},As).

ca_fun({var,Var},{Vars,O}) -> 
  case proplists:get_value(Var,Vars) of
    undefined -> V = list_to_atom("\$"++integer_to_list(length(Vars)+1));
    V -> ok
  end,
  {[{Var,V}|Vars],O++[V]};
ca_fun({Type,Val},{Vars,O}) -> 
  assert_type(Type,Val),
  {Vars,O++[Val]}.

assert_type(Type,Val) -> 
  case lists:member(Type,[integer,atom,string]) of
    true -> ok;
    false-> exit({bad_type,{Type,Val}})
  end.
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parser  
%% accepts strings like;
%%   "a","a:b","a:b/2","a:b(X,y)",
%%   "a:b(X,Y)when is_record(X,rec) and Y==0, (X==z)"
%%   "a:b->stack", "a:b(X)whenX==2->return"
%% returns
%%   {atom(M),atom(F),list(Arg)|integer(Arity),list(Guard),list(Action)}
parse(Str) ->
  {Body,Guard,Action} = assert(split_fun(Str),{split_string,Str}),
  {M,F,A}             = assert(body_fun(Body),{parse_body,Str}),
  Guards              = assert(guards_fun(Guard),{parse_guards,Str}),
  Actions             = assert(actions_fun(Action),{parse_actions,Str}),
  {M,F,A,Guards,Actions}.

%% split the input string in three parts; body, guards, actions
%% we parse them separately
split_fun(Str) ->
  fun() ->
      % strip off the actions, if any
      case re:run(Str,"^(.+)->\\s*([a-z;]+)\$",[{capture,[1,2],list}]) of
        {match,[St,Action]} -> ok;
        nomatch             -> St=Str,Action=""
      end,
      % strip off the guards, if any
      case re:run(St,"^(.+)when(.+)\$",[{capture,[1,2],list}]) of
        {match,[S,Guard]} -> ok;
        nomatch           -> S=St,Guard=""
      end,
      % add a wildcard F, if Body is just an atom (presumably a module)
      case re:run(S,"^[a-zA-Z0-9_]+\$") of
        nomatch -> Body=S;
        _       -> Body=S++":'_'"
      end,
      {Body,Guard,Action}
  end.

body_fun(Str) ->
  fun() ->
      {done,{ok,Toks,1},[]} = erl_scan:tokens([],Str++". ",1),
      case erl_parse:parse_exprs(Toks) of
        {ok,[{op,1,'/',{remote,1,{atom,1,M},{atom,1,F}},{integer,1,Ari}}]} ->
          {M,F,Ari};
        {ok,[{call,1,{remote,1,{atom,1,M},{atom,1,F}},Args}]} ->
          {M,F,[arg(A) || A<-Args]};
        {ok,[{remote,1,{atom,1,M},{atom,1,F}}]} ->
          {M,F,'_'};
        {ok,_} ->
          exit(this_is_too_confusing)
     end
  end.

guards_fun(Str) -> 
  fun() ->
      case Str of
        "" -> [];
        _ -> 
          {done,{ok,Toks,1},[]} = erl_scan:tokens([],Str++". ",1),
          {ok,Guards} = erl_parse:parse_exprs(Toks),
          [guard(G)||G<-Guards]
      end
  end.

guard({call,1,{atom,1,G},Args}) -> {G,[arg(A) || A<-Args]};   % function
guard({op,1,Op,One,Two})        -> {Op,guard(One),guard(Two)};% unary op
guard({op,1,Op,One})            -> {Op,guard(One)};           % binary op
guard(Guard)                    -> arg(Guard).                % variable

arg({cons,_,_,_})   -> exit({syntax_error,"lists not allowed in arg list"});
arg({tuple,1,Args}) -> {tuple,[arg(A)||A<-Args]};
arg({T,1,Var})      -> {T,Var}.

actions_fun(Str) ->
  fun() ->
      string:tokens(Str,";")
  end.

assert(Fun,Tag) ->
  try Fun()
  catch 
    _:{_,{error,{1,erl_parse,L}}}-> exit({{syntax_error,lists:flatten(L)},Tag});
    _:R                          -> exit({R,Tag})
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ad-hoc unit testing

unit() ->
  lists:foldr(
    fun(Str,O)->[unit(fun transform/1,Str)|O]end,[],
    [{"a",
      [{{a,'_','_'},{[],[],[]}}]}
     ,{"a->stack",
       [{{a,'_','_'},{[],[],[{message,{process_dump}}]}}]}
     ,{"a:b",
       [{{a,b,'_'},{[],[],[]}}]}
     ,{"a:b->return",
       [{{a,b,'_'},{[],[],[{return_trace}]}}]}
     ,{"a:b/2",
       [{{a,b,2},{[],[],[]}}]}
     ,{"a:b/2->return",
       [{{a,b,2},{[],[],[{return_trace}]}}]}
     ,{"a:b(X,Y)",
       [{{a,b,2},{['$1','$2'],[],[]}}]}
     ,{"a:b(X,X)",
       [{{a,b,2},{['$1','$1'],[],[]}}]}
     ,{"a:b(X,X) -> return;stack",
       [{{a,b,2},{['$1','$1'],[],[{return_trace},{message,{process_dump}}]}}]}
     ,{"a:b(X,y)",
       [{{a,b,2},{['$1',y],[],[]}}]}
     ,{"a:b(X,1)",
       [{{a,b,2},{['$1',1],[],[]}}]}
     ,{"a:b(X,\"foo\")",
       [{{a,b,2},{['$1',"foo"],[],[]}}]}
     ,{"a:b(X,y)when is_atom(Y)",
       unbound_variable}
     ,{"x:c([string])",
       {syntax_error,"lists not allowed in arg list"}}
     ,{"x(s)",
       this_is_too_confusing}
     ,{"x:c(S)when S==x;S==y",
       {syntax_error,"syntax error before: S"}}
     ,{"x:y(z)->bla",
       unknown_action}
     ,{"a:b(X,y)when not is_atom(X)",
       [{{a,b,2},{['$1',y],[{'not',{is_atom,'$1'}}],[]}}]}
     ,{"a:b(X,y)when not is_atom(X) -> return",
       [{{a,b,2},{['$1',y],[{'not',{is_atom,'$1'}}],[{return_trace}]}}]}
     ,{"a:b(X,y)when element(1,X)==foo, (X==z)",
       [{{a,b,2},{['$1',y],[{'==',{element,1,'$1'},foo},{'==','$1',z}],[]}}]}
    ,{"a:b(X,Y)when X==1,Y=/=a",
       [{{a,b,2},{['$1','$2'],[{'==','$1',1},{'=/=','$2',a}],[]}}]}
    ]).


unit(Method,{Str,MS}) ->
  try MS=Method(Str),Str
  catch 
    _:{MS,_} -> Str;
    C:R      -> {C,R,Str,erlang:get_stacktrace()}
  end.
