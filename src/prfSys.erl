%%%-------------------------------------------------------------------
%%% File    : prfSys.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector of system info
%%%
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfSys).

-export([collect/1,config/2]).

-record(cst,{beam, load, os={os:type(),os:version()}, node=node()}).

-define(LOG(T), prf:log(process_info(self()),T)).

%%% returns {State, Data}
collect(init) ->
  Cst = #cst{},
  collect(Cst#cst{beam = init_get_beam(Cst), load=init_get_load(Cst)});
collect(Cst) ->
  {Cst, {?MODULE, [{node, Cst#cst.node}, {now, now()}|data(Cst)]}}.

config(State,_ConfigData) -> State.

data(Cst) ->
  sysi()++stat()++mem()++get_beam(Cst)++get_load(Cst).

-define(SIS,[allocated_areas,process_count,system_version,system_architecture]).
sysi() -> sysi(?SIS,[]).
sysi([],O) -> O;
sysi([Si|Sis],O) ->
  try sysi(Sis,[{Si,erlang:system_info(Si)}|O])
  catch _:_ -> sysi(Sis,O)
  end.

-define(STATS,[run_queue,runtime,wall_clock,reductions,garbage_collection]).
stat() -> stat(?STATS, []).
stat([], O) -> O;
stat([Stat|Stats], O) ->
  try stat(Stats, [{Stat, erlang:statistics(Stat)}|O])
  catch _:_ -> stat(Stats, O)
  end.

mem() ->
  try erlang:memory()
  catch _:_ -> []
  end.

%% /proc/self/stat
%% [pid,comm,state,ppid,pgrp,session,tty_nr,tpgid,flags,
%%  minflt,cminflt,majflt,cmajflt,utime,stime,cutime,cstime,
%%  priority,nice,0,itrealvalue,starttime,vsize,rss|_]
get_beam(#cst{beam={linux, FD}}) -> %% FD is /proc/self/stat
  {ok,PSI} = file:pread(FD,0,2000),
  [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,BS|_] = string:tokens(PSI," "),
  [{beamsize,list_to_integer(BS)}];
get_beam(#cst{beam={sunos, FD}}) -> %% FD is /proc/self/psinfo
  {ok,PSI} = file:pread(FD,0,2000),
  <<_:44/binary,Vsz:32,_/binary>> = PSI,
  [{beamsize, Vsz*1024}];
get_beam(#cst{beam={unix, Port, Cmd}}) when port(Port) ->
  Port ! {self(), {command, Cmd}},
  receive
    {Port, {data, Data}} -> 
      case string:tokens(Data, " \n") of
        [Int] -> [{beamsize, list_to_integer(Int)*1024}];
        X -> ?LOG({nai,X}), [{beamsize, 0}]
      end
  after 
    500 -> []
  end;
get_beam(_) -> [].

init_get_beam(#cst{os={{unix,linux},{2,SubV,_}}}) when SubV >= 6 ->
  {ok, FD} = file:open("/proc/self/stat", [read]),
  {linux,FD};
init_get_beam(#cst{os={{unix,sunos},_}}) ->
  {ok, FD} = file:open("/proc/self/psinfo", [read,raw,binary]), 
  {sunos,FD};
init_get_beam({unix,_}) ->
  {unix,
   open_port({spawn,"sh -s prfSys 2>&1"},[stream]),
   "ps -o 'vsz' -p "++os:getpid()++" | tail -1\n"};
init_get_beam(_) -> 
  no.

init_get_load(#cst{os={{unix,linux},{2,SubV,_}}}) when SubV >= 6 ->
  {ok,FD} = file:open("/proc/stat", [read,raw]),
  {linux,FD};
init_get_load(_) ->
  no.

get_load(#cst{load={linux,FD}}) ->
  {ok,3} = file:position(FD, 3),
  {ok,Str} = file:read(FD, 100),
  {ok,[User,Nice,Kern,Idle,IoWt],_} = io_lib:fread("~d~d~d~d~d", Str),
  [{user,User},{nice_user,Nice},{kernel,Kern},{idle,Idle},{io_wait,IoWt}];
get_load(_) ->
  case [ok || {os_mon,_} <- application:which_applications()] of
    [ok] ->
      try 
	{_,L1,L2,_} = cpu_sup:util([detailed]),
	L1++L2
      catch _:_ -> []
      end;
    [] ->
      []
  end.
%%       /proc/[number]/stat
%%       /proc/[number]/smaps
%%       /proc/[number]/statm
%%               size       total program size
%%               resident   resident set size
%%               share      shared pages
%%               text       text (code)
%%               lib        library
%%               data       data/stack
%%               dt         dirty pages (unused in Linux 2.6)
%%       /proc/meminfo
%%       /proc/net/dev
%%       /proc/stat
%%       /proc/uptime
%%       /proc/vmstat
