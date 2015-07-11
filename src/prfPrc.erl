%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : prfPrc.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector for per-process data
%%%
%%% Created :  8 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
%% collect info about an erlang process.
%% uses erlang:process_info/2
%%
%% registered_name      atom | []
%% initial_call         {M,F,A}
%% current_function     {M,F,A}
%% last_calls           [{M,F,A}]
%% reductions           integer()
%% message_queue_len    integer()
%% memory               bytes
%% stack_size           bytes
%% heap_size            bytes
%% total_heap_size      bytes
%%%-------------------------------------------------------------------
-module(prfPrc).

-export([collect/1,config/2]).
-export([pid_info/1,pid_info/2]).

-record(cst,{items=6
             , max_procs=3000
             , extra_items = []
             , old_info=[]}).

%%% reductions,message_queue_len,memory
%%% current_function,initial_call,registered_name
%%% N.B. 'reductions' is reductions/sec

-define(SORT_ITEMS,[reductions,memory,message_queue_len]).
-define(INFO_ITEMS,[current_function,initial_call,registered_name,last_calls,
                    stack_size,heap_size,total_heap_size]).
-define(TAGS,?SORT_ITEMS++?INFO_ITEMS).

config(init,Config) -> config(init_cst(),Config);
config(State,{items,Items}) when is_number(Items) -> State#cst{items=Items};
config(State,{max_procs,MP}) when is_number(MP) -> State#cst{max_procs=MP};
config(State,{add_extra,M,F}) -> add_extra(State,{M,F});
config(State,{rm_extra,M,F}) -> rm_extra(State,{M,F});
config(State,_ConfigData) -> State.

add_extra(S=#cst{extra_items=X},MF) -> S#cst{extra_items=lists:usort([MF|X])}.
rm_extra (S=#cst{extra_items=X},MF) -> S#cst{extra_items=X--[MF]}.

%%% returns {State, Data}
collect(init) ->
  collect(init_cst());
collect(Cst = #cst{items=Items})->
  Info = get_info(Cst),
  {Cst#cst{old_info=Info}, {?MODULE,select(Cst,Info,Items)}}.

init_cst() ->
  #cst{old_info=get_info(#cst{})}.

get_info(Cst) ->
  case Cst#cst.max_procs < erlang:system_info(process_count) of
    true -> {prfTime:ts(),[]};
    false-> {prfTime:ts(),[{P,pid_info(P,?SORT_ITEMS)}||P<-lists:sort(processes())]}
  end.

%%% Dreds, Dmems, Mems and Msgqs are sorted lists of pids
%%% PidInfo is a sorted list of {Pid,Info}
%%% Info is a list of tagged tuples {atom(),number()}

select(Cst = #cst{old_info={Then,Olds}},{Now,Curs},Items) ->
  {DredL,DmemL,MemL,MsgqL} = topl(Olds,Curs,outf(Then,Now,Items),empties()),
  PidInfo = lists:usort([I || {_,I} <-lists:append([DredL,DmemL,MemL,MsgqL])]),
  [{node,node()},
   {now,prfTime:ts()},
   {dreds,e1e2(DredL)},
   {dmem,e1e2(DmemL)},
   {mem,e1e2(MemL)},
   {msgq,e1e2(MsgqL)},
   {info,complete(PidInfo,Cst)}].

e1e2(List) -> [E || {_,{E,_}} <- List].

complete(List,#cst{extra_items=X}) ->
  [{Pid,
    Info++
      pid_info(Pid,?INFO_ITEMS)++
      extra_items(Pid,X)} || {Pid,Info} <- List].

topl([],_,_,Out) -> Out;
topl(_,[],_,Out) -> Out;
topl(Os=[{Po,_}|_],[{Pc,_}|Cs],Outf,Out) when Pc<Po -> topl(Os,Cs,Outf,Out);
topl([{Po,_}|Os],Cs=[{Pc,_}|_],Outf,Out) when Po<Pc -> topl(Os,Cs,Outf,Out);
topl([{P,Io}|Os],[{P,Ic}|Cs],Outf,Out) -> topl(Os,Cs,Outf,Outf(P,Io,Ic,Out)).

empties() -> {[],[],[],[]}.

outf(Then,Now,Items) ->
  NowDiff = timer:now_diff(Now,Then)/1000000,
  fun(P,Io,Ic,Out) -> out(P,NowDiff,Io,Ic,Out,Items) end.

out(P,NowDiff,Io,Ic,O={Odred,Omem,Odmem,Omsgq},Items) ->
  try
    Dred = dred(NowDiff,Io,Ic),
    Dmem = dmem(NowDiff,Io,Ic),
    Info = {P,[{dreductions,Dred},{dmemory,Dmem}|Ic]},
    {new_topl(Odred,{Dred,Info},Items),
     new_topl(Odmem,{Dmem,Info},Items),
     new_topl(Omem,{mem(Ic),Info},Items),
     new_topl(Omsgq,{msgq(Ic),Info},Items)}
  catch
    _:_ -> O
  end.

new_topl(Top,{Item,_},_Items) when 0 =:= Item; 0.0 =:= Item ->
  Top;
new_topl(Top,El,Items) when length(Top) < Items ->
  lists:sort([El|Top]);
new_topl(Top,El,_Items) ->
  case El < hd(Top) of
    true -> Top;
    false-> tl(lists:sort([El|Top]))
  end.

dred(NowDiff,Io,Ic)-> (red(Ic)-red(Io))/NowDiff.
dmem(NowDiff,Io,Ic)-> abs((mem(Ic)-mem(Io))/NowDiff).

red([]) -> 0;
red([{reductions,Reds}|_]) -> Reds.

mem([]) -> 0;
mem([_,{memory,Mem}|_]) -> Mem.

msgq([]) -> 0;
msgq([_,_,{message_queue_len,Msgq}]) -> Msgq.

%% callbacks for app-specific info
extra_items(Pid,Items) ->
  lists:append([extra_item(Pid,I) || I <- Items]).

extra_item(Pid,{M,F}) when is_pid(Pid) ->
  try M:F(Pid)
  catch _:_ -> []
  end;
extra_item(_,_) ->
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pid_info/1

pid_info(Pid) when is_pid(Pid) ->
  pid_info(Pid,?TAGS).

pid_info(Pid,Tags) when is_list(Tags) ->
  [pidinfo(Pid,T) || T <- Tags].

pidinfo(Pid,Tag) ->
  {Getter,Default} = pidinfo(Tag),
  {Tag, try Getter(Pid) catch _:_ -> Default end}.

pidinfo(Type = stack_size) ->
  {fun(Pid) -> 8*element(2,process_info(Pid, Type))end,
   0};
pidinfo(Type = heap_size) ->
  {fun(Pid) -> 8*element(2,process_info(Pid, Type))end,
   0};
pidinfo(Type = total_heap_size) ->
  {fun(Pid) -> 8*element(2,process_info(Pid, Type))end,
   0};
pidinfo(Type = last_calls) ->
  {fun(Pid) ->
       case process_info(Pid,Type) of
         {_,false} -> process_flag(Pid,save_calls,16),[];
         {_,Calls} -> lists:usort(Calls)
       end
   end,
   []};
pidinfo(Type = registered_name) ->
  {fun(Pid) ->
       case process_info(Pid, Type) of
         []       -> [];
         {Type,N} -> N
       end
   end,
  []};
pidinfo(Type = initial_call) ->
  {fun(Pid) ->
       case process_info(Pid, Type) of
         {Type,{proc_lib,init_p,5}} ->
           case proc_lib:translate_initial_call(Pid) of
             {dets,init,2}     -> pinf_dets(Pid);
             {disk_log,init,2} -> pinf_disk_log(Pid);
             IC                -> IC
           end;
         {Type,IC} -> IC
       end
   end,
   []};
pidinfo(Type) ->
  {fun(Pid) -> {Type,I} = process_info(Pid, Type), I end,
   []}.

pinf_dets(Pid) ->
  case dets:pid2name(Pid) of
    {ok,Dets} -> {dets, Dets};
    undefined -> undefined_dets_table
  end.

pinf_disk_log(Pid) ->
  case disk_log:pid2name(Pid) of
    {ok, Log} -> {disk_log, Log};
    undefined -> undefined_disk_log
  end.
