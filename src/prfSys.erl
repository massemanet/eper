%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : prfSys.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector of system info
%%%
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
%% collects info about the OS and the Erlang system.
%% * emulator info
%%   call the BIFs memory/0, statistics/1 and system_info/1
%% * OS info
%%   if OS is Linux 2.6 or greater;
%%     - read from /proc/stat, /proc/meminfo (and /proc/net/dev ?)
%%     - read from /proc/self/stat (and /proc/self/statm ?)
%%   elseif os_mon is started;
%%     - call memsup:get_memory_data/0 and cpu_sup:cpu_sup:util([detailed])
%%   else if OS is unix
%%     - run the ps command in a port
%%   else
%%     - return an empty list
%%
%% returns a list of tagged tuples
%%
%% tag                  [unit]    source
%% node                 [atom()]  erlang:node()
%% now                  [now()]   erlang:now()
%% procs                [count]   erlang:system_info(process_count)
%% context_switches     [count/s] erlang:statistics(context_switches)
%% gcs                  [count/s] erlang:statistics(garbage_collection)
%% gc_reclaimed         [byte/s]  erlang:statistics(garbage_collection)
%% io_in                [byte/s]  erlang:statistics(io)
%% io_out               [byte/s]  erlang:statistics(io)
%% reductions           [count/s] erlang:statistics(reductions)
%% run_queue            [count]   erlang:statistics(run_queue)
%% total                [byte]    erlang:memory()
%% processes            [byte]    erlang:memory()
%% processes_used       [byte]    erlang:memory()
%% system               [byte]    erlang:memory()
%% atom                 [byte]    erlang:memory()
%% atom_used            [byte]    erlang:memory()
%% binary               [byte]    erlang:memory()
%% code                 [byte]    erlang:memory()
%% ets                  [byte]    erlang:memory()
%% user                 [frac]    /proc/stat
%% nice                 [frac]    /proc/stat
%% kernel               [frac]    /proc/stat
%% idle                 [frac]    /proc/stat
%% iowait               [frac]    /proc/stat
%% ctxt                 [frac]    /proc/stat
%% beam_user,           [frac]    /proc/self/stat
%% beam_kernel,         [frac]    /proc/self/stat
%% beam_vss             [byte]    /proc/self/stat
%% beam_rss             [pages]   /proc/self/stat
%% beam_minflt          [count/s] /proc/self/stat
%% beam_majflt          [count/s] /proc/self/stat
%% total_ram            [byte]    /proc/meminfo

%% f(C),f(Data),{C,Data}=prfSys:collect(Cst),f(Cst),Cst=C,Data.

-module(prfSys).
-export([collect/1,config/2]).

-record(cst,{strategy=strategy(), node=node(), total_ram=0, cores=1,
             cache=[], now=now()}).

-define(RATES,[context_switches,gcs,gc_reclaimed,io_in,io_out,reductions,
               user,nice,kernel,idle,iowait,
               beam_user,beam_kernel,beam_minflt,beam_majflt]).

%%% returns {State, Data}
collect(init) ->
  collect(init_cst(#cst{}));
collect(Cst) ->
  Data = data(Cst),
  {new_cst(Cst,Data), {?MODULE, rates(Cst,Data)}}.

config(State,_ConfigData) -> State.

data(Cst) -> constants(Cst)++stats()++os_info(Cst#cst.strategy).

constants(#cst{node=Node,total_ram=Total_ram,cores=Cores}) ->
  [{node, Node},{total_ram,Total_ram},{cores,Cores}].

stats() ->
  Procs = erlang:system_info(process_count),
  {Ctxt,0} = erlang:statistics(context_switches),
  {GCs,GCwords,0} = erlang:statistics(garbage_collection),
  {{input,IoIn},{output,IoOut}} = erlang:statistics(io),
  {Reds,_} = erlang:statistics(reductions),
  RunQ = erlang:statistics(run_queue),

  [{now, now()},
   {procs,Procs},
   {context_switches,Ctxt},
   {gcs,GCs},
   {gc_reclaimed,GCwords*4},
   {io_in,IoIn},
   {io_out,IoOut},
   {reductions,Reds},
   {run_queue,RunQ} |
   erlang:memory()].

new_cst(Cst,Data) ->
  Cst#cst{cache=[D || {Tag,_}=D <- Data, lists:member(Tag,?RATES)],
          now=lks(now,Data)}.

rates(#cst{cache=Cache,now=Now},Data) ->
  [diff(Cache,D,delta_t(Now,Data)) || D <- Data].

diff(Cache,{Tag,Val},DeltaT)->
  try {Tag,(Val-lks(Tag,Cache))/DeltaT}
  catch
    no_tag -> {Tag,Val};
    _:_ -> {Tag,0}
  end.

delta_t(Then,Data) ->
  timer:now_diff(lks(now,Data),Then)/1000000.

lks(Tag,List) ->
  case lists:keysearch(Tag,1,List) of
    {value,{Tag,Val}} -> Val;
    false -> throw(no_tag)
  end.

init_cst(Cst = #cst{strategy={linux,_}}) ->
  Cst#cst{total_ram = total_ram(), cores = cores(Cst#cst.strategy)};
init_cst(Cst) ->
  Cst.

strategy() ->
  Os_mon_p = [ok||{os_mon,_,_}<-application:which_applications()],
  case os:type() of
    {unix,linux}            -> {linux,init_linux()};
    _ when Os_mon_p == [ok] -> {os_mon,[]};
    {unix,_}                -> {ps,init_ps()};
    _                       -> {none,[]}
  end.

%% OS info
%% only the 'linux' (i.e. linux 2.6 or higher) strategy implemented
-record(fds,{proc_stat,proc_self_stat}).

os_info({linux,#fds{proc_stat=FDs,proc_self_stat=FDss}}) ->
  proc_stat(FDs)++proc_self_stat(FDss);
os_info(_) ->
  [].

proc_stat(FDs) ->
%%user nice kernel idle iowait irq softirq steal
  {ok,Str} = file:pread(FDs,0,200),
  [User,Nice,Kernel,Idle,Iowait] =
    case string:tokens(Str," \n") of
      ["cpu",I1,I2,I3,I4,I5|_] -> [I1,I2,I3,I4,I5];
      _                        -> [0,0,0,0,0]
    end,
  lists:zip([user,nice,kernel,idle,iowait],
            [to_sec(J) || J <- [User,Nice,Kernel,Idle,Iowait]]).

proc_self_stat(FDss) ->
%%% pid,comm,state,ppid,pgrp,session,tty_nr,tpgid,flags,
%%% minflt,cminflt,majflt,cmajflt,utime,stime,cutime,cstime,
%%% priority,nice,num_threads,itrealvalue,starttime,vsize,rss
  {ok,Str} = file:pread(FDss,0,200),
  {Minflt,Majflt,Utime,Stime,Vsize,Rss} =
    case string:tokens(Str," ") of
      [_,_,_,_,_,_,_,_,_,I10,_,I12,_,I14,I15,_,_,_,_,_,_,_,I23,I24|_] ->
        {I10,I12,I14,I15,I23,I24};
      _ ->
        {0,0,0,0,0,0}
    end,
  lists:zip([beam_user,beam_kernel,beam_vss,beam_rss,beam_minflt,beam_majflt],
            [to_sec(Utime),to_sec(Stime),to_int(Vsize),
             to_int(Rss), %% in pages...
             to_int(Minflt),to_int(Majflt)]).

to_sec(J) ->
  to_int(J)/100. %should use a better transform jiffies->secs

to_int(J) -> list_to_integer(J).

init_linux() ->
  {ok,FDs} = file:open("/proc/stat",[read,raw]),
  {ok,FDss} = file:open("/proc/self/stat",[read,raw]),
  #fds{proc_stat=FDs,proc_self_stat=FDss}.

cores({linux,#fds{proc_stat=Proc_stat}}) ->
  {ok,Str} = file:pread(Proc_stat,0,1000),
  Toks = string:tokens(Str,"\n"),
  case length(lists:takewhile(fun(S)->lists:prefix("cpu",S) end,Toks)) of
    1 -> 1;
    M -> M-1
  end;
cores(_) ->
  1.

total_ram() ->
  case file:open("/proc/meminfo",[read,raw]) of
    {ok,FD} ->
      try {ok,Str} = file:pread(FD,0,30),
          ["MemTotal:",T,"kB"|_] = string:tokens(Str," \n"),
          list_to_integer(T)*1024
      catch _:_ -> 0
      after file:close(FD)
      end;
    _ -> 0
  end.

init_ps() ->
  [].
