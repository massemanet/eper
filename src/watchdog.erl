%%%-------------------------------------------------------------------
%%% File    : sysWatchdog.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description : 
%%%
%%% Created : 11 Mar 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(watchdog).

-export([start/0,stop/0]).

-import(error_logger,[info_report/1,error_report/1]).

-record(ld, {jailed=[],				%jailed pids
	     subscribers=[],			%where to send our reports
	     prf_targ,				%prfTarg:subscribe return val
	     prfSys,				%
	     prfProc,				%
	     lines=5,				%# of displayed procs
	     gc_trig=500,			%gc time [ms]
	     heap_trig=1024*256,		%heap size [words]
	     mem_hw_trig=200,			%beam proc size [MB]
	     cpu_trig=0.95}).			%cpu load [frac]

%% constants
timeout(restart) -> 5000;			%  5 sec
timeout(release) -> 3000.			%  3 sec
-define(max_jailed, 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> 
  Init = fun(D) -> fun() -> init(D) end end,       % mmm.... haskell....
  {Pid,Ref} = erlang:spawn_monitor(Init(self())), 
  receive
    {Pid,ok} -> erlang:demonitor(Ref,[flush]),Pid;
    {'DOWN',Ref,_,Pid,OldPid} -> OldPid
  end.

stop() -> 
  try ?MODULE ! stop
  after ok
  end.
  
init(Daddy) ->
  try register(?MODULE,self()), 
      Daddy ! {self(),ok}
  catch _:_ -> exit(whereis(?MODULE))
  end,
  loop(start_monitor(#ld{})).

loop(LD) when ?max_jailed < length(LD#ld.jailed) ->
  %% we take a timeout when enough pids are jailed. conservative is good.
  erlang:start_timer(timeout(restart), self(), restart),
  stop_monitor(),
  flush(),
  loop(LD#ld{jailed=[]});
loop(LD) ->
  receive
    %% quit
    stop -> 
      ok;
    %% set configs
    {set_lines,N} when integer(N) -> 
      loop(LD#ld{lines = N});		        %number of displayed processes
    {set_gc_trig,N} when integer(N) -> 
      loop(restart(LD#ld{gc_trig=N}));		%msec of gc
    {set_heap_trig,N} when integer(N) -> 
      loop(restart(LD#ld{heap_trig=N*16#40000})); %N is heap size in MB
    {set_mem_hw_trig,N} when integer(N) -> 
      loop(LD#ld{mem_hw_trig=N});		%beam proc size in MB
    {set_cpu_trig,F}when float(F) -> 
      loop(LD#ld{cpu_trig=F});			%cpu load in frac
    %% data from prfTarg
    {{data,_},[{prfSys,PrfSys_data}]} ->
      case check_triggers(LD#ld{prfSys=PrfSys_data}) of
	{none,NewLD} -> ok;
	{Trigger,NewLD} -> self() ! {triggered,Trigger}
      end,
      loop(NewLD);
    %% triggers
    {triggered,mem} -> 
      report(mem,LD,[]),
      loop(LD);
    {triggered,cpu} -> 
      report(cpu,LD,[]),
      loop(LD);
    {monitor,Pid,Tag,Data} ->
      case lists:member(Pid, LD#ld.jailed) of
	true -> loop(LD);
	false ->
	  report(Tag,LD,pi(Pid)++data(Data)),
	  erlang:start_timer(timeout(release), self(), {release, Pid}),
	  loop(LD#ld{jailed=[Pid|LD#ld.jailed]})
      end;
    %% restarting after timeout
    {timeout, _, restart} -> 
      start_monitor(LD),
      loop(LD#ld{jailed=[]});
    %% release a pid from jail
    {timeout, _, {release, Pid}} ->
      loop(LD#ld{jailed = LD#ld.jailed--[Pid]});
    X ->
      error_report([{?MODULE,unexpected},X]),
      loop(LD)
  end.

start_monitor(LD) ->
  erlang:system_monitor(self(), sysmons(LD)),
  LD#ld{prf_targ=prfTarg:subscribe(node(),self(),[prfSys])}.

sysmons(#ld{gc_trig=GC,heap_trig=Heap}) ->
  [{long_gc,GC},{large_heap,Heap},busy_port,busy_dist_port].

stop_monitor() ->
  erlang:system_monitor(undefined).


report(Trigger,LD,Info) ->
  Report = make_report(Trigger,LD,Info),
  [Sub(Report) || Sub <- LD#ld.subscribers].

make_report(mem,LD,[]) ->
  [{?MODULE,memory}]++maybe_procs(LD)++generic_report(LD);
make_report(cpu,LD,[]) ->
  [{?MODULE,cpu}]++maybe_procs(LD)++generic_report(LD);
make_report(Tag,LD,Info) -> 
  [{?MODULE,Tag}]++info(Info)++generic_report(LD).

generic_report(#ld{prfSys = SysI}) ->
  Vsz = lks(vsz,SysI,0),
  Cpu = lks(user,SysI,0),
  [{cpu,Cpu},{vsz,Vsz}].

maybe_procs(#ld{lines = 0}) -> [].

info(Info) -> [].

data(Port) when port(Port) -> porti(Port);
data(Data) when list(Data) -> mnorm(Data).

mnorm([]) -> [];
mnorm([{T=heap_block_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([{T=mbuf_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([{T=stack_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([{T=heap_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([H|R]) -> [H|mnorm(R)].

restart(LD) ->
  stop_monitor(),
  start_monitor(LD).

-define(INFS,[message_queue_len,current_function,initial_call,registered_name]).
pi(Pid) -> pi(Pid, ?INFS, [{pid,Pid}]).
pi(_Pid, [], O) -> O;
pi(Pid, [Tag|Tags], O) ->
  case process_info(Pid, Tag) of
    undefined -> pi(Pid, Tags, O);
    [] -> pi(Pid, Tags, O);
    Val -> pi(Pid, Tags, [Val|O])
  end.

flush() ->    
  receive {monitor,_,_,_} -> flush()
  after 0 -> ok
  end.

check_triggers(LD = #ld{prfSys=SysI, mem_hw_trig=HW, cpu_trig=CPU}) ->
  case {lks(user,SysI,0),lks(vsz,SysI,0)} of
    {Load,_Vsz} when Load > CPU -> {cpu,LD};
    {_Load,Vsz} when Vsz > HW -> {mem,LD#ld{mem_hw_trig=Vsz}};
    _ -> {none,LD}
  end.

norm([], _) -> [];
norm(L, Max) when Max =< 0 -> L;
norm([{Tag,X}|T], Max) when number(X) -> [{Tag,X/Max}|norm(T,Max)];
norm([{X,Tag}|T], Max) when number(X) -> [{X/Max,Tag}|norm(T,Max)].

-define(PORTIS,[name,id,connected,input,output]).
porti(Port) -> 
  [{port,Port}|porti(Port, ?PORTIS)].

porti(_Port,[]) -> [];
porti(Port,[Porti|Portis]) ->
  case erlang:port_info(Port,Porti) of
    undefined -> porti(Port,Portis);
    X -> [X|porti(Port,Portis)]
  end.

lks(Tag,List,Def) ->
  case lists:keysearch(Tag,1,List) of
    {value,{Tag,Val}} -> Val;
    false -> Def
  end.
