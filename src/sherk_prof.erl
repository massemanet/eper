%%%-------------------------------------------------------------------
%%% File    : sherk_prof.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 30 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_prof).

-export([go/3]).

-include("log.hrl").

go(Msg, Seq, initial)        -> go(Msg, Seq, init());
go(end_of_trace, Seq, State) -> terminate(Seq,State), State;
go(Msg, Seq, State)          -> handler(Msg) ! {msg,Seq,Msg}, State.

init() ->
  ?log([{starting,?MODULE}]),
  sherk_ets:new(sherk_prof),
  {start,prfTime:ts()}.

terminate(Seq,{start,Start}) ->
  ?log([{finishing,sherk_prof},
        {seq,Seq},
        {time,timer:now_diff(prfTime:ts(),Start)/1000000},
        {procs,length(ets:match(sherk_prof,{{handler,'$1'},'_'}))}]),
  TermFun = fun({{handler,_},Pid},_) -> Pid ! quit; (_,_) -> ok end,
  ets:foldl(TermFun,[],sherk_prof).

handler({spawn,_,{Pid,_},_,_}) -> assert_handler(Pid);
handler({_,{Pid,_},_,_}) -> assert_handler(Pid).

assert_handler(Pid) ->
  case sherk_ets:lup(sherk_prof,{handler,Pid}) of
    [] ->
      P = spawn_link(fun handler/0),
      P ! {pid,Pid},
      ets:insert(sherk_prof,{{handler,Pid},P}),
      P;
    HandlerPid ->
      HandlerPid
  end.

-record(s,{gc=no,fd=no,in=no,ts,pid,stack=[]}).

handler() ->
  receive
    {pid,Pid} -> hloop(#s{pid=Pid});
    quit -> ok
  end.

hloop(S) ->
  receive
    {msg,_Seq,{Tag,_,Info,TS}} -> hloop(hand(Tag,Info,TS,S));
    quit -> ok
  end.

hand(Tag,Info,TS,S) ->
  %%io:fwrite("~p ~p~n~p~n",[Tag,Info,S#s.stack]),
  case {Tag,Info} of
    {out,0}               -> (out(Tag,S,TS))#s{fd=yes};
    {in,0}                -> in(Tag,S#s{fd=no},TS);
    {gc_start,_}          -> (out(Tag,S,TS))#s{gc=yes};
    {gc_end,_}            -> in(Tag,S#s{gc=no},TS);
    {out,MFA}             -> (out(Tag,stk(S,arity(MFA)),TS))#s{in=no};
    {in,MFA}              -> in(Tag,stk(S#s{in=yes},arity(MFA)),TS);
    {call,MFA}            -> call(S,TS,arity(MFA));
    {return_to,undefined} -> S;
    {return_to,MFA}       -> retu(S,TS,arity(MFA));
    {spawn,{_,MFA}}       -> spwn(S,TS,arity(MFA));
    {exit,_}              -> exIt(S,TS);
    {_,_}                 -> S
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(Tag,S,TS) ->
  %% left MFA at top of stack
  upd({{total,sched},Tag}),
  upd({{pid,sched},S#s.pid,Tag}),
  case is_running(S) of
    false ->
      ?log([not_running,{tag,Tag},{state,S}]),
      S;
    true ->
      case S#s.ts of
        undefined ->
          ?log([no_time,{state,S}]);
        _ ->
          T = timer:now_diff(TS,S#s.ts),
          MFA = hd(S#s.stack),
          upd({total,time}, T),
          upd({{pid,time}, S#s.pid}, T),
          upd({{func,time}, MFA}, T),
          upd({{func,time}, S#s.pid, MFA}, T),
          upd({{stack,time}, S#s.pid, S#s.stack}, T)
      end,
      S#s{ts=TS}
  end.

in(Tag,S,TS) ->
  %% entered MFA at top of stack
  upd({{total,sched},Tag}),
  upd({{pid,sched},S#s.pid,Tag}),
  case is_running(S) of
    false -> ?log([not_running,{tag,Tag},{state,S}]),S;
    true ->  S#s{ts=TS}
  end.

call(S,TS,MFA) ->
  in(call,push_stack(MFA, out(call,S,TS)),TS).

retu(S,TS,MFA) ->
  in(return,pop_stack(MFA, out(return,S,TS)),TS).

spwn(S,_TS,MFA) ->
  push_stack(MFA, S).

exIt(S,TS) ->
  out(exit,S,TS).

push_stack(MFA,S) ->
  case S#s.stack of
    [MFA|_] ->
      S;
    Stack ->
      Trunced = truncate([MFA|Stack]),
      upd({{func, calls}, MFA}),
      upd({{func, calls}, S#s.pid, MFA}),
      upd({{stack, calls}, S#s.pid, Trunced}),
      S#s{stack=Trunced}
  end.

pop_stack(MFA,S) ->
  case lists:member(MFA,S#s.stack) of
    false ->
      ?log([dropped_headless_stack,{mfa,MFA},{state,S}]),
      erase_bad_stack(S),
      S#s{stack=[MFA]};
    true ->
      S#s{stack=lists:dropwhile(fun(Mfa) -> MFA =/= Mfa end, S#s.stack)}
  end.

truncate([MFA|Stack] = X) ->
  case lists:member(MFA,Stack) of
    true ->
      {H,[MFA|T]} = lists:splitwith(fun(E)->E/=MFA end,Stack),
      case lists:prefix(H,T) of
        true -> [MFA|T];
        false -> X
      end;
    false -> X
  end.

erase_bad_stack(S) ->
  mdl({{{stack,calls}, S#s.pid, '_'},'_'}),
  mdl({{{stack,time}, S#s.pid, '_'}, '_'}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


stk(S,MFA) ->
  case S#s.stack of
    [] -> S#s{stack=[arity(MFA)]};
    _ -> S
  end.

arity({M,F,A}) when is_list(A) -> {M,F,length(A)};
arity(MFA) -> MFA.

is_running(#s{gc=no,fd=no,in=yes}) -> true;
is_running(_) -> false.

upd(Key) ->
  upd(Key,1).
upd(Key,Inc) ->
  sherk_ets:upd(sherk_prof,Key,Inc).

mdl(Pat) ->
  ets:match_delete(sherk_prof,Pat).
