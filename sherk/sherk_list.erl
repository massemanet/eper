%%%-------------------------------------------------------------------
%%% File    : sherk_list.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 21 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_list).

-export([go/1]).
-import(lists,[sublist/2,reverse/1,sort/1,flatten/1,sort/1]).
-define(LOG(T), sherk:log(process_info(self()),T)).

go(Tag) ->
    ?LOG([{starting,Tag}]),
    Start = now(),
    X = do(Tag),
    ?LOG([{finishing,Tag},
          {length,length(X)},
	  {time,timer:now_diff(now(),Start)/1000000}]),
    X.

do(perf) ->
    Tot = sherk_ets:lup(sherk_prof,{total,time}),
    L = reverse(sort(ets:match(sherk_prof,{{{pid,time},'$2'},'$1'}))),
    [[str(P),str(tag(P)),T,percent(T,Tot)] || [T,P] <- L];
do({call,PidStr}) ->
    [{_,Pid}] = ets:lookup(sherk_prof,PidStr),
    Tot = sherk_ets:lup(sherk_prof,{{pid,time}, Pid}),
    TMFAs = reverse(sort(mfas(Pid))),
    [[str(MFA),calls(Pid,MFA),percent(T,Tot)] || [T,MFA] <- TMFAs].

str(X) -> flatten(io_lib:fwrite("~p",[X])).

tag(P) -> 
    case sherk_ets:lup(sherk_scan,P) of
        {M,F,As} when is_list(As) -> mangle(M,F,As);
        X -> X
    end.

mangle(M,F,As) ->
    {M,F,length(As)}.

percent(_,0) -> 0;
percent(A,B) -> round(100*A/B).

mfas(Pid) -> ets:match(sherk_prof,{{{func,time}, Pid, '$2'}, '$1'}).

calls(Pid,MFA) -> 
    case sherk_ets:lup(sherk_prof,{{func, calls}, Pid, MFA}) of
        [] -> 0;
        N -> N
    end.
            
