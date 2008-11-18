%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : prfPrc.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector for per-process data
%%%
%%% Created :  8 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfPrc).

-export([collect/1,config/2]).

-record(cst,{now=now(),items=19}).

%%% reductions,message_queue_len,memory
%%% current_function,initial_call,registered_name
%%% N.B. 'reductions' is reductions/sec

-define(SORT_ITEMS,[reductions,memory,message_queue_len]).
-define(INFO_ITEMS,[current_function,initial_call,registered_name]).
config(State,_ConfigData) -> State.

%%% returns {State, Data}
collect(init) -> 
  catch ets:delete(?MODULE),
  ets:new(?MODULE,[ordered_set,named_table]),
  collect(#cst{});
collect(Cst) -> 
  Now = now(),
  NowDiff = timer:now_diff(Now,Cst#cst.now)/1000000,
  T0 = empty_toplists(Cst#cst.items),
  TopLists = lists:foldl(fun(P,A) -> topl(P,A,NowDiff) end, T0, processes()),
  {Cst#cst{now=Now},{?MODULE,out(Now,TopLists)}}.

topl(P,TopLists,NowDiff) ->
  try [pinf(P,T) || T <- ?SORT_ITEMS] of
    [Vred,Vmem,Vmsg] ->
      RedDiff = red_diff(P,Vred,NowDiff),
      update_toplists(P,TopLists,RedDiff,Vmem,Vmsg)
  catch 
    _:_ -> 
      catch ets:delete(?MODULE,P), TopLists
  end.

empty_toplists(Items) ->
  Dummies = lists:duplicate(Items,{}),
  {Dummies,Dummies,Dummies}.

red_diff(P,Reds,NowDiff) ->
  try ets:lookup(?MODULE,P) of
    [] -> 0;
    [{P,OReds}] -> (Reds-OReds)/NowDiff
  after 
    ets:insert(?MODULE,{P,Reds})
  end.

update_toplists(P,{TopRed,TopMem,TopMsg},Vred,Vmem,Vmsg) ->
  {update_toplist({Vred,[P,Vred,Vmem,Vmsg]},TopRed),
   update_toplist({Vmem,[P,Vred,Vmem,Vmsg]},TopMem),
   update_toplist({Vmsg,[P,Vred,Vmem,Vmsg]},TopMsg)}.

update_toplist(El,Top) ->
  case El < hd(Top) of
    true -> Top;
    false-> tl(lists:sort([El|Top]))
  end.

out(Now,{RedList,MemList,MsgList}) -> 
  [{now,Now},
   {process_count,erlang:system_info(process_count)},
   {reds,complete(RedList)},
   {mem,complete(MemList)},
   {msg,complete(MsgList)}].

complete(List) ->
  lists:foldl(fun({_,El},Acc)->postf(El,Acc) end,[],List).

postf(El,Acc) ->
  try 
    P = hd(El),
    InfoItems = [{I,pinf(P,I)} || I <- ?INFO_ITEMS],
    [lists:zip([pid|?SORT_ITEMS],El)++InfoItems|Acc]
  catch 
    _:_ -> Acc
  end.

pinf(Pid, Type = registered_name) ->
  case process_info(Pid, Type) of
    [] -> [];
    {Type,Val} -> Val
  end;
pinf(Pid, Type = initial_call) ->
  case process_info(Pid, Type) of
    {Type,{proc_lib,init_p,5}} -> 
      case proc_lib:translate_initial_call(Pid) of
	{dets,init,2}->{dets, element(2, dets:pid2name(Pid))};
	IC -> IC
      end;
    {Type,{dets, do_open_file, 11}}->pinf_dets(Pid);%gone in R12
    {Type, Val} -> Val
  end;
pinf(Pid, Type) -> 
  {Type, Val} = process_info(Pid, Type),
  Val.

pinf_dets(Pid) ->
  {dets, element(2, dets:pid2name(Pid))}.
