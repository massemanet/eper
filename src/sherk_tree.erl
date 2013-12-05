%%%-------------------------------------------------------------------
%%% File    : sherk_tree.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 21 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_tree).

-export([go/1]).

-define(LOG(T), sherk:log(process_info(self()),T)).

go(procs) ->
    Tot = sherk_ets:lup(sherk_prof,{total,time}),
    PTs = ets:match(sherk_prof,{{{pid,time},'$1'},'$2'}),
    L = [{[reg(P),P],[garb(P),T]} || [P,T] <- PTs],
    Pf = fun(Key,Data,Out) -> procsf(Tot,Key,Data,Out) end,
    make_tree(L, Pf, fun sortpf/1);

go({callgraph,PidStr}) ->
    case sherk_ets:lup(sherk_prof,PidStr) of
        [] -> [];
        Pid -> callgraph(Pid)
    end.

callgraph(Pid) ->
    Tot = sherk_ets:lup(sherk_prof,{{pid,time},Pid}),
    L = ets:match(sherk_prof,{{{stack,time},Pid,'$1'},'$2'}),
    LS = [{lists:reverse(Stak),{T,called(Pid,Stak)}} || [Stak,T] <- L],
    Gf = fun(Key,Data,Out) -> graphf(Tot,Key,Data,Out) end,
    make_tree(LS, Gf, fun sortgf/1).

sortpf(L) ->
    lists:sort(fun({_,[_,_,_,T1],_},{_,[_,_,_,T2],_}) -> T2<T1 end,L).

procsf(_Tot,Pid,[Garb,Time],[]) when is_pid(Pid) ->
    [sherk:to_str(Pid),1,Garb,Time];
procsf(_Tot,_Rg,[Garb,Time],[Tag,N,G,T]) ->
    [Tag,N+1,G+Garb,T+Time];
procsf(_Tot,Reg,[Garb,Time],[]) ->
    [sherk:to_str(Reg),1,Garb,Time].

sortgf(L) ->
    lists:sort(fun({_,[_,_,_,CT1],_},{_,[_,_,_,CT2],_}) -> CT2<CT1 end, L).

graphf(_Tot,MFA,{Time,Calls},[]) ->
    [sherk:to_str(MFA),Calls,Time,Time];
graphf(_Tot,_,{Time,_},[Tag,Calls,T,CT]) ->
    [Tag,Calls,T,CT+Time].

called(Pid,Stak) ->
    sherk_ets:lup(sherk_prof,{{stack,calls},Pid,Stak}).

reg(P) ->
    case ets:lookup(sherk_scan,P) of
        [] -> unknown;
        [{P,{M,F,A}}] when is_list(A) -> {M,F,length(A)};
        [{P,R}] -> R
    end.

garb(Pid) ->
    case sherk_ets:lup(sherk_prof,{{pid,sched},Pid,gc_start}) of
        [] -> 0;
        V -> V
    end.

%%%merg({R,T,P},[{Ti,R,TP}|Tail]) -> [{T+Ti,R,[{T,P}|TP]}|Tail];
%%%merg({R,T,P},O) -> [{T,R,[{T,P}]}|O].

%%%subtree(TP,T) ->
%%%    [{[pct(Ti,T),garb(Pid),to_str(Pid)],[]} || {Ti,Pid} <- TP].

make_tree(L,DataF,SortF) ->
    F = fun({Key,Data},A) -> tree_ins(Key,Data,DataF,SortF,A) end,
    lists:foldl(F, [], lists:sort(L)).

%%% node() := {Key,Data,[nodes()]}
tree_ins([],_,_,_,[]) ->
    [];
tree_ins([K|Ks],Data,Df,Sf,Tree) ->
    case lists:keysearch(K,1,Tree) of
        false ->
            Sf([{K,Df(K,Data,[]),tree_ins(Ks,Data,Df,Sf,[])}|Tree]);
        {value,{K,OData,OTree}} ->
            Sf(krep(Tree,{K,Df(K,Data,OData),tree_ins(Ks,Data,Df,Sf,OTree)}))
    end.

krep(List,El) -> lists:keyreplace(element(1,El),1,List,El).
