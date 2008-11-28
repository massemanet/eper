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
-export([pid_info/1,pid_info/2]).

-record(cst,{old_info=get_info()}).

%%% reductions,message_queue_len,memory
%%% current_function,initial_call,registered_name
%%% N.B. 'reductions' is reductions/sec

-define(ITEMS,19).
-define(SORT_ITEMS,[reductions,memory,message_queue_len]).
-define(INFO_ITEMS,[current_function,initial_call,registered_name]).
-define(TAGS,?SORT_ITEMS++?INFO_ITEMS).

config(State,_ConfigData) -> State.

%%% returns {State, Data}
collect(init) -> 
  collect(#cst{});
collect(Cst) -> 
  Info = get_info(),
  {Cst#cst{old_info=Info}, {?MODULE,select(Cst#cst.old_info,Info)}}.

get_info() ->
  %% hardcoded 999, because it's really not a good idea to up it
  %% trust me...
  case 999 < erlang:system_info(process_count) of
    true -> {now(),[]};
    false-> {now(),[{P,pid_info(P,?SORT_ITEMS)}||P<-processes()]} 
  end.

%%% Dreds, Dmems, Mems and Msgqs are sorted lists of pids
%%% PidInfo is a sorted list of {Pid,Info}
%%% Info is a list of tagged tuples {atom(),number()}

select({Then,Olds},{Now,Curs}) ->
  {DredL,DmemL,MemL,MsgqL} = topl(Olds,Curs,outf(Then,Now),empties()),
  PidInfo = lists:usort([I || {_,I} <- lists:append([DredL,DmemL,MemL,MsgqL])]),
  [{dreds,e1e2(DredL)},
   {dmem,e1e2(DmemL)},
   {mem,e1e2(MemL)},
   {msgq,e1e2(MsgqL)},
   {info,complete(PidInfo)}].

e1e2(List) -> [E || {_,{E,_}} <- List].

complete(List) ->
  [{Pid,Info++pid_info(Pid,?INFO_ITEMS)}||{Pid,Info}<-List].

topl([],_,_,Out) -> Out;
topl(_,[],_,Out) -> Out;
topl(Os=[{Po,_}|_],[{Pc,_}|Cs],Outf,Out) when Pc<Po -> topl(Os,Cs,Outf,Out);
topl([{Po,_}|Os],Cs=[{Pc,_}|_],Outf,Out) when Po<Pc -> topl(Os,Cs,Outf,Out);
topl([{P,Io}|Os],[{P,Ic}|Cs],Outf,Out) -> topl(Os,Cs,Outf,Outf(P,Io,Ic,Out)).

empties() -> {[],[],[],[]}.

outf(Then,Now) ->  
  NowDiff = timer:now_diff(Now,Then)/1000000,
  fun(P,Io,Ic,Out) -> out(P,NowDiff,Io,Ic,Out) end.

out(P,NowDiff,Io,Ic,O={Odred,Omem,Odmem,Omsgq}) -> 
  try
    Dred = dred(NowDiff,Io,Ic),
    Dmem = dmem(NowDiff,Io,Ic),
    Info = {P,[{dreductions,Dred},{dmemory,Dmem}|Ic]}, 
    {new_topl(Odred,{Dred,Info}),
     new_topl(Odmem,{Dmem,Info}),
     new_topl(Omem,{mem(Ic),Info}),
     new_topl(Omsgq,{msgq(Ic),Info})}
  catch 
    _:_ -> O
  end.

new_topl(Top,{Item,_}) when 0 =:= Item; 0.0 =:= Item -> 
  Top;
new_topl(Top,El) when length(Top) < ?ITEMS -> 
  lists:sort([El|Top]);
new_topl(Top,El) -> 
  case El < hd(Top) of
    true -> Top;
    false-> tl(lists:sort([El|Top]))
  end.

dred(NowDiff,Io,Ic)-> (red(Ic)-red(Io))/NowDiff.
dmem(NowDiff,Io,Ic)-> (mem(Ic)-mem(Io))/NowDiff.

red([]) -> 0;
red([{reductions,Reds}|_]) -> Reds.

mem([]) -> 0;
mem([_,{memory,Mem}|_])	-> Mem.

msgq([]) -> 0;
msgq([_,_,{message_queue_len,Msgq}]) -> Msgq.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pid_info/1

pid_info(Pid) when is_pid(Pid) ->
  pid_info(Pid,?TAGS).

pid_info(Pid,Tags) when is_list(Tags) -> 
  try [pidinfo(Pid,T) || T <- Tags]
  catch _:_ -> []
  end.

pidinfo(Pid, Type = registered_name) ->
  case process_info(Pid, Type) of
    [] -> {Type,[]};
    XX -> XX
  end;
pidinfo(Pid, Type = initial_call) ->
  case process_info(Pid, Type) of
    {Type,{proc_lib,init_p,5}} -> 
      case proc_lib:translate_initial_call(Pid) of
	{dets,init,2} -> {Type,{dets, element(2, dets:pid2name(Pid))}};
	IC -> {Type,IC}
      end;
    {Type,{dets, do_open_file, 11}}->{Type,pinf_dets(Pid)};%gone in R12
    XX -> XX
  end;
pidinfo(Pid, Type) -> 
  process_info(Pid, Type).

pinf_dets(Pid) ->
  {dets, element(2, dets:pid2name(Pid))}.
