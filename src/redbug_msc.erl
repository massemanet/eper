%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 Mar 2010 by Mats Cronqvist <masse@kreditor.se>

%% msc - match spec compiler
%% transforms a string to a call trace expression;
%% {MFA,MatchSpec} == {{M,F,A},{Head,Cond,Body}}


-module('redbug_msc').
-author('Mats Cronqvist').

-export([transform/1]).
-export([unit_loud/0,unit_quiet/0]).

-define(is_string(Str),
        (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

transform(E) ->
  compile(parse(to_string(E))).

to_string(A) when is_atom(A)    -> atom_to_list(A);
to_string(S) when ?is_string(S) -> S;
to_string(X)                    -> exit({illegal_input,X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compiler
%% returns {{Module,Function,Arity},[{Head,Cond,Body}],[Flag]}
%% i.e. the args to erlang:trace_pattern/3

compile({M,F,Ari,[],Actions}) when is_integer(Ari) ->
  compile({M,F,lists:duplicate(Ari,{var,'_'}),[],Actions});
compile({M,OF,As,Gs,Actions}) ->
  {F,A,Flags} = chk_fa(OF,As),
  {Vars,Args} = compile_args(As),
  {{M,F,A}, [{Args,compile_guards(Gs,Vars),compile_acts(Actions)}], Flags}.

chk_fa('X',_) -> {'_','_',[global]};
chk_fa('_',_) -> {'_','_',[local]};
chk_fa(F,'_') -> {F,'_',[local]};
chk_fa(F,As)  -> {F,length(As),[local]}.

compile_acts(As) ->
  [ac_fun(A)|| A <- As].

ac_fun("stack") -> {message,{process_dump}};
ac_fun("return")-> {exception_trace};
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

unpack_var({tuple,Es},Vars) ->
  {list_to_tuple([unpack_var(E,Vars)||E<-Es])};
unpack_var({list,Es},Vars) ->
  [unpack_var(E,Vars)||E<-Es];
unpack_var({var,Var},Vars) ->
  case proplists:get_value(Var,Vars) of
    undefined -> exit({unbound_variable,Var});
    V -> V
  end;
unpack_var({Op,As},Vars) when is_list(As) ->
  unpack_op(Op,As,Vars);
unpack_var({Op,V1,V2},Vars) ->
  unpack_op(Op,[V1,V2],Vars);
unpack_var({Type,Val},_) ->
  assert_type(Type,Val),
  Val.

compile_args('_') ->
  {[{'$_','$_'}],'_'};
compile_args(As) ->
  lists:foldl(fun ca_fun/2,{[],[]},As).

ca_fun({list,Es},{Vars,O}) ->
  {Vs,Ps} = ca_fun_list(Es,Vars),
  {Vs,O++[Ps]};
ca_fun({tuple,Es},{Vars,O}) ->
  {Vs,Ps} = ca_fun_list(Es,Vars),
  {Vs,O++[list_to_tuple(Ps)]};
ca_fun({var,'_'},{Vars,O}) ->
  {Vars,O++['_']};
ca_fun({var,Var},{Vars,O}) ->
  V =
    case proplists:get_value(Var,Vars) of
      undefined -> list_to_atom("\$"++integer_to_list(length(Vars)+1));
      X -> X
    end,
  {[{Var,V}|Vars],O++[V]};
ca_fun({Type,Val},{Vars,O}) ->
  assert_type(Type,Val),
  {Vars,O++[Val]}.

ca_fun_list(Es,Vars) ->
  cfl(Es,{Vars,[]}).

cfl([],O) -> O;
cfl([E|Es],{V0,P0}) when is_list(Es) ->
  {V,P} = ca_fun(E,{V0,[]}),
  cfl(Es,{lists:usort(V0++V),P0++P});
cfl([E1|E2],{V0,P0}) ->
  %% non-proper list / tail match
  {V1,[P1]} = ca_fun(E1,{V0,[]}),
  {V2,[P2]} = ca_fun(E2,{lists:usort(V0++V1),[]}),
  {lists:usort(V0++V1++V2),P0++[P1|P2]}.

assert_type(Type,Val) ->
  case lists:member(Type,[integer,atom,string,char]) of
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
      {St,Action} =
        case re:run(Str,"^(.+)->\\s*([a-z;,]+)\\s*\$",[{capture,[1,2],list}]) of
          {match,[Z,A]} -> {Z,A};
          nomatch       -> {Str,""}
        end,
      % strip off the guards, if any
      {S,Guard} =
        case re:run(St,"^(.+[\\s)])+when\\s(.+)\$",[{capture,[1,2],list}]) of
          {match,[Y,G]} -> {Y,G};
          nomatch       -> {St,""}
        end,
      % add a wildcard F, if Body is just an atom (presumably a module)
      Body =
        case re:run(S,"^\\s*[a-zA-Z0-9_]+\\s*\$") of
          nomatch -> S;
          _       -> S++":'_'"
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
        {ok,[{call,1,{remote,1,{atom,1,M},{var,1,'_'}},Args}]} ->
          {M,'_',[arg(A) || A<-Args]};
        {ok,[{call,1,{remote,1,{atom,1,M},{var,1,_}},Args}]} ->
          {M,'X',[arg(A) || A<-Args]};
        {ok,[{remote,1,{atom,1,M},{atom,1,F}}]} ->
          {M,F,'_'};
        {ok,[{remote,1,{atom,1,M},{var,1,'_'}}]} ->
          {M,'_','_'};
        {ok,[{remote,1,{atom,1,M},{var,1,_}}]} ->
          {M,'X','_'};
        {ok,C} ->
          exit({this_is_too_confusing,C})
     end
  end.

guards_fun(Str) ->
  fun() ->
      case Str of
        "" -> [];
        _ ->
          {done,{ok,Toks,1},[]} = erl_scan:tokens([],Str++". ",1),
          {ok,Guards} = erl_parse:parse_exprs(disjunct_guard(Toks)),
          [guard(G)||G<-Guards]
      end
  end.

%% deal with disjunct guards by replacing ';' with 'orelse'
disjunct_guard(Toks) ->
  [case T of {';',1} -> {'orelse',1}; _ -> T end||T<-Toks].

guard({call,1,{atom,1,G},Args}) -> {G,[arg(A) || A<-Args]};   % function
guard({op,1,Op,One,Two})        -> {Op,guard(One),guard(Two)};% unary op
guard({op,1,Op,One})            -> {Op,guard(One)};           % binary op
guard(Guard)                    -> arg(Guard).                % variable

arg({op,_,'++',{string,_,Str},Var}) -> {list,arg_list(consa(Str,Var))};
arg({call,1,F,Args}) -> guard({call,1,F,Args});
arg({nil,_})         -> {list,[]};
arg(L={cons,_,_,_})  -> {list,arg_list(L)};
arg({tuple,_,Args})  -> {tuple,[arg(A)||A<-Args]};
arg({T,_,Var})       -> {T,Var}.

consa([],T)     -> T;
consa([C],T)    -> {cons,1,{char,1,C},T};
consa([C|Cs],T) -> {cons,1,{char,1,C},consa(Cs,T)}.

arg_list({cons,_,H,T}) -> [arg(H)|arg_list(T)];
arg_list({nil,_})      -> [];
arg_list(V)            -> arg(V).

actions_fun(Str) ->
  fun() ->
      string:tokens(Str,";,")
  end.

assert(Fun,Tag) ->
  try Fun()
  catch
    _:{this_is_too_confusing,C}  -> exit({syntax_error,{C,Tag}});
    _:{_,{error,{1,erl_parse,L}}}-> exit({syntax_error,{lists:flatten(L),Tag}});
    _:R                          -> exit({R,Tag,erlang:get_stacktrace()})
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ad-hoc unit testing

unit_loud() ->
  lists:foreach(fun(R)->io:fwrite("~p~n",[R])end,unit()).

unit_quiet() ->
  [R||R<-unit(),is_tuple(R)].

unit() ->
  lists:foldr(
    fun(Str,O)->[unit(Str)|O]end,[],
    [{"a when element(1,'$_')=/=b",
      {{a,'_','_'},
       [{'_',[{'=/=',{element,1,'$_'},b}],[]}],[local]}}
     ,{"erlang when tl(hd('$_'))=={}",
       {{erlang,'_','_'},
        [{'_',[{'==',{tl,{hd,'$_'}},{{}}}],[]}],
        [local]}}
     ,{"a:b when element(1,'$_')=/=c",
       {{a,b,'_'},
        [{'_',[{'=/=',{element,1,'$_'},c}],[]}],[local]}}
     ,{"a",
       {{a,'_','_'},[{'_',[],[]}],[local]}}
     ,{"a->stack",
       {{a,'_','_'},[{'_',[],[{message,{process_dump}}]}],[local]}}
     ,{"a:b",
       {{a,b,'_'},[{'_',[],[]}],[local]}}
     ,{"a:b->return ",
       {{a,b,'_'},[{'_',[],[{exception_trace}]}],[local]}}
     ,{"a:b/2",
       {{a,b,2},[{['_','_'],[],[]}],[local]}}
     ,{"a:b/2->return",
       {{a,b,2},[{['_','_'],[],[{exception_trace}]}],[local]}}
     ,{"a:b(X,Y)",
       {{a,b,2},[{['$1','$2'],[],[]}],[local]}}
     ,{"a:b(_,_)",
       {{a,b,2},[{['_','_'],[],[]}],[local]}}
     ,{"a:b(X,X)",
       {{a,b,2},[{['$1','$1'],[],[]}],[local]}}
     ,{"a:b(X,y)",
       {{a,b,2},[{['$1',y],[],[]}],[local]}}
     ,{"a:foo()when a==b",
       {{a,foo,0},[{[],[{'==',a,b}],[]}],[local]}}
     ,{"a:foo when a==b",
       {{a,foo,'_'},[{'_',[{'==',a,b}],[]}],[local]}}
     ,{"a:b(X,1)",
       {{a,b,2},[{['$1',1],[],[]}],[local]}}
     ,{"a:b(X,\"foo\")",
       {{a,b,2},[{['$1',"foo"],[],[]}],[local]}}
     ,{"x:y({A,{B,A}},A)",
       {{x,y,2},[{[{'$1',{'$2','$1'}},'$1'],[],[]}],[local]}}
     ,{"x:y(A,[A,{B,[B,A]},A],B)",
       {{x,y,3},[{['$1',['$1',{'$2',['$2','$1']},'$1'],'$2'],[],[]}],[local]}}
     ,{"a:b(X,y)when is_atom(Y)",
       unbound_variable}
     ,{"x:c([string])",
       {{x,c,1},[{[[string]],[],[]}],[local]}}
     ,{"x(s)",
       syntax_error}
     ,{"x-s",
       syntax_error}
     ,{"x:c(S)when S==x;S==y",
       {{x,c,1},
        [{['$1'],[{'orelse',{'==','$1',x},{'==','$1',y}}],[]}],
        [local]}}
     ,{"x:c(S)when (S==x)or(S==y)",
       {{x,c,1},
        [{['$1'],[{'or',{'==','$1',x},{'==','$1',y}}],[]}],
        [local]}}
     ,{"a:b(X,Y)when is_record(X,rec) and (Y==0), (X==z)",
       {{a,b,2},
        [{['$1','$2'],
          [{'and',{is_record,'$1',rec},{'==','$2',0}},{'==','$1',z}],[]}],
        [local]}}
     ,{"x:y(z)->bla",
       unknown_action}
     ,{"a:b(X,y)when not is_atom(X)",
       {{a,b,2},[{['$1',y],[{'not',{is_atom,'$1'}}],[]}],[local]}}
     ,{"a:b(X,Y)when X==1,Y=/=a",
      {{a,b,2},[{['$1','$2'],[{'==','$1',1},{'=/=','$2',a}],[]}],[local]}}
     ,{"a:b(X,y)when not is_atom(X) -> return",
       {{a,b,2},
        [{['$1',y],[{'not',{is_atom,'$1'}}],[{exception_trace}]}],
        [local]}}
     ,{"a:b(X,y)when element(1,X)==foo, (X==z)",
       {{a,b,2},
        [{['$1',y],[{'==',{element,1,'$1'},foo},{'==','$1',z}],[]}],
        [local]}}
     ,{"a:b(X,X) -> return;stack",
       {{a,b,2},
        [{['$1','$1'],[],[{exception_trace},{message,{process_dump}}]}],
        [local]}}
     ,{"x:y(A,[A,B,C])when A==B,is_atom(C)",
       {{x,y,2},
        [{['$1',['$1','$2','$3']],
          [{'==','$1','$2'},{is_atom,'$3'}],
          []}],
        [local]}}
     ,{"x:y([A,B,C])when A=/=B,is_atom(C)",
       {{x,y,1},
        [{[['$1','$2','$3']],[{'=/=','$1','$2'},{is_atom,'$3'}],[]}],
        [local]}}
     ,{"a:b([A,B,T])when B==T",
       {{a,b,1},
        [{[['$1','$2','$3']],[{'==','$2','$3'}],[]}],
        [local]}}
     ,{"x:y([C|{D}])when is_atom(C)",
       {{x,y,1},
        [{[['$1'|{'$2'}]],[{is_atom,'$1'}],[]}],
        [local]}}
     ,{"lists:reverse(\"ab\"++_)",
       {{lists,reverse,1},
        [{[[97,98|'_']],[],[]}],
        [local]}}
     ,{"lists:reverse(\"ab\"++C)when 3<length(C)",
       {{lists,reverse,1},
        [{[[97,98|'$1']],[{'<',3,{length,'$1'}}],[]}],
        [local]}}
     ,{"a:b([$a,$b|C])",
       {{a,b,1},[{[[97,98|'$1']],[],[]}],
        [local]}}
     ,{"a:_(a)",
       {{a,'_','_'},[{[a],[],[]}],
        [local]}}
     ,{"a:_",
       {{a,'_','_'},[{'_',[],[]}],
        [local]}}
     ,{"a:_->return",
       {{a,'_','_'},[{'_',[],[{exception_trace}]}],
        [local]}}
     ,{"erlang:_({A}) when hd(A)=={}",
       {{erlang,'_','_'},[{[{'$1'}],[{'==',{hd,'$1'},{{}}}],[]}],
        [local]}}
     ,{"a:X([]) -> return,stack",
       {{a,'_','_'},
        [{[[]],[],[{exception_trace},{message,{process_dump}}]}],
        [global]}}
     ,{"lists:X([a])",
       {{lists,'_','_'},[{[[a]],[],[]}],
        [global]}}
     ,{"lists:X(A) when is_list(A)",
       {{lists,'_','_'},[{['$1'],[{is_list,'$1'}],[]}],
        [global]}}
     ,{"lists:X",
       {{lists,'_','_'},[{'_',[],[]}],
        [global]}}
    ]).

unit({Str,MS}) ->
  try MS=transform(Str),Str
  catch
    _:{MS,_}       -> Str;
    _:{{MS,_},_}   -> Str;
    _:{{MS,_},_,_} -> Str;
    _:R            -> {Str,R,MS}
  end.
