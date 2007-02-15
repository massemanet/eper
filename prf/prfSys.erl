%%%-------------------------------------------------------------------
%%% File    : prfSys.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector of system info
%%%
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfSys).

-export([collect/1,config/2]).

-record(cst,{beam, os}).

-define(LOG(T), prf:log(process_info(self()),T)).

%%% returns {State, Data}
collect(init) ->
  Os = os:type(),
  collect(#cst{beam = get_beam(Os), os = Os});
collect(Cst = #cst{os = Os, beam = Beam}) ->
  {Cst, {?MODULE, [{node, node()}, {now, now()}|data(Os, Beam)]}}.

config(State,_ConfigData) -> State.

data(Os, Beam) ->
  case [x || {os_mon,_,_} <- application:which_applications()] of
    [x] -> sysi()++stat()++mem()++cpu()++smem()++get_beam(Os, Beam);
    [] -> sysi()++stat()++mem()++get_beam(Os, Beam)
  end.

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

cpu() ->
  try 
    {_,L1,L2,_} = cpu_sup:util([detailed]),
    L1++L2
  catch _:_ -> []
  end.

smem() ->
  try
    {PhysMem, FreeMem, _} = memsup:get_memory_data(),
    [{physmem, PhysMem},{freemem, FreeMem}]
  catch _:_ -> []
  end.

%% /proc/self/stat
%% [pid,comm,state,ppid,pgrp,session,tty_nr,tpgid,flags,
%%  minflt,cminflt,majflt,cmajflt,utime,stime,cutime,cstime,
%%  priority,nice,0,itrealvalue,starttime,vsize,rss|_]
get_beam({unix,linux}, FD) -> 
  {ok,PSI} = file:pread(FD,0,2000),
  [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,BS|_] = string:tokens(PSI," "),
  [{beamsize,list_to_integer(BS)}];
get_beam({unix,sunos}, FD) -> %% FD is /proc/self/psinfo
  {ok,PSI} = file:pread(FD,0,2000),
  <<_:44/binary,Vsz:32,_/binary>> = PSI,
  [{beamsize, Vsz*1024}];
get_beam({unix,_}, {Port, Cmd}) when port(Port) ->
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
get_beam(_, _) -> [].

get_beam({unix,linux}) -> %% FD is /proc/self/psinfo
  {ok, FD} = file:open("/proc/self/stat", [read]),
  FD;
get_beam({unix,sunos}) -> %% FD is /proc/self/psinfo
  {ok, FD} = file:open("/proc/self/psinfo", [read,raw,binary]), 
  FD;
get_beam({unix,_}) ->
  {open_port({spawn,"sh -s prfSys 2>&1"},[stream]),
   "ps -o 'vsz' -p "++os:getpid()++" | tail -1\n"};
get_beam(_) -> no.
