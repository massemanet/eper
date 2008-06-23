%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : gperfConsumer.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 17 May 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(gperfConsumer).

-export([init/1, terminate/1, tick/2, collectors/0, config/2]).

-record(ld, {node,data=[],mem_max=0,net_max=0,load_max=0}).

-include("log.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collectors() -> [prfSys].

init(Node) -> #ld{node = Node}.

terminate(_LD) -> ok.

tick(LD, Data) -> 
  NLD = update_ld(LD, Data),
  gperf ! {tick, {ltime(NLD#ld.data),info(NLD)}},
  NLD.

config(LD,{mem,Mem}) ->
  %% gui unit is MByte
  LD#ld{mem_max=Mem*1024*1024};
config(LD,{net,Net}) ->
  %% gui unit is kByte/s
  LD#ld{net_max=Net*1024};
config(LD,{cpu,Cpu}) ->
  %% gui unit is %
  LD#ld{load_max=Cpu/100.0};
config(LD,dbg) ->
  dump_ld(LD),
  LD.

info(LD) ->
  [load(LD#ld.load_max,LD#ld.data),
   mem(LD#ld.mem_max,LD#ld.data),
   net(LD#ld.net_max,LD#ld.data)].

dump_ld(LD) ->
  ?log(lists:zip(record_info(fields,ld),tl(tuple_to_list(LD)))).

update_ld(LD,[]) -> 
  LD#ld{data=[]};
update_ld(LD,[{prfSys,Sys}]) -> 
  LD#ld{data=Sys};
update_ld(LD,[X]) -> 
  ?log({unrecognized_data,X}), LD.

ltime(Sys) ->
  case T=lks(now,Sys) of
    {_,_,_} -> {_,Time} = calendar:now_to_local_time(T),Time;
    Bad -> ?log({bad_time,Bad}),no_time
  end.

load(Max,Sys) ->
  Beam = lks(beam_user,Sys),
  User = lks(user,Sys),
  Kern = lks(kernel,Sys),
  IoWt = lks(iowait,Sys),
  [max(1,dv(Beam,Max)),
   max(1,dv(User,Max)),
   max(1,dv(User+Kern,Max)),
   max(1,dv(User+Kern+IoWt,Max))].

mem(Max,Sys) ->
  [max(1,dv(lks(beam_vss,Sys),Max)),
   max(1,dv(lks(total,Sys),Max)),
   max(1,dv(lks(ets,Sys)+lks(processes,Sys),Max)),
   max(1,dv(lks(processes,Sys),Max))].

net(Max,Sys) -> 
  [max(1,dv(lks(io_in,Sys),Max)),
   max(1,dv(lks(io_out,Sys),Max))].

lks(Tag,List) ->
  case lists:keysearch(Tag,1,List) of
    {value, {Tag,Val}} -> Val;
    _ -> 0
  end.

dv(_,0) -> 0;
dv(A,B) -> A/B.

max(R,V) when V < R -> V;
max(R,_) -> R.
