%%%-------------------------------------------------------------------
%%% File    : prfSys.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector of system info
%%%
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfSys).

-export([collect/1]).

-record(cst,{beam, os}).

-include("prf.hrl").

%%% returns {State, Data}
collect(init) ->
    Os = os:type(),
    collect(#cst{beam = get_beam(Os), os = Os});
collect(Cst = #cst{os = Os, beam = Beam}) ->
    {Cst, {?MODULE, [{node, node()}, {now, now()}|data(Os, Beam)]}}.

data(Os, Beam) ->
    sysi()++stat()++mem()++cpu()++smem()++get_beam(Os, Beam).

-define(SIS,[allocated_areas,process_count,system_version,system_architecture]).
sysi() -> sysi(?SIS,[]).
sysi([],O) -> O;
sysi([Si|Sis],O) ->
    case catch erlang:system_info(Si) of
	{'EXIT',_} -> sysi(Sis,O);
	Val -> sysi(Sis,[{Si,Val}|O])
    end.

-define(STATS,[run_queue,runtime,wall_clock,reductions,garbage_collection]).
stat() -> stat(?STATS, []).
stat([], O) -> O;
stat([Stat|Stats], O) ->
    case catch erlang:statistics(Stat) of
	{'EXIT', _} -> stat(Stats, O);
	Val -> stat(Stats, [{Stat, Val}|O])
    end.

mem() ->
    case catch erlang:memory() of
	{'EXIT', _} -> [];
	Val -> Val
    end.

cpu() ->
    case catch cpu_sup:util([detailed]) of
	{_,L1,L2,_} when list(L1), list(L2) -> L1++L2;
	_ -> []
    end.

smem() ->
    case catch memsup:get_memory_data() of
	{'EXIT', _} -> [];
	{PhysMem, FreeMem, _} -> [{physmem, PhysMem},{freemem, FreeMem}]
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
	?TICK div 4 -> []
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
