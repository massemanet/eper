%%%-------------------------------------------------------------------
%%% File    : prfHost.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  2 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfHost).

-export([start/3,start/4,stop/1,config/3]).
-export([loop/1]).				%internal

-record(ld, {node, server=[], collectors, config=[], 
             proxy, consumer, consumer_data, data=[]}).

-define(LOOP, ?MODULE:loop).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runs in the shell

start(Name,Node,Consumer) -> start(Name,Node,Consumer,no_proxy).
start(Name,Node,Consumer,Proxy)
  when is_atom(Name),is_atom(Node),is_atom(Consumer),is_atom(Proxy) -> 
  assert_proxy(Proxy),
  SpawnFun = fun()->init(Consumer,Node,Proxy) end,
  case whereis(Name) of
    undefined -> register(Name, spawn_link(SpawnFun));
    Pid -> Pid
  end.

stop(Name) -> 
  case whereis(Name) of
    Pid when is_pid(Pid) -> Name ! {self(),stop}, receive stopped -> ok end;
    _ -> ok
  end.

config(Name,Type,Data) ->
  case whereis(Name) of
    Pid when is_pid(Pid) -> Pid ! {config,{Type,Data}};
    _ -> {Name,not_running}
  end.

assert_proxy(no_proxy) -> ok;
assert_proxy(Node) ->
  erlang:set_cookie(Node,watchdog),
  case net_adm:ping(Node) of
    pong -> ok;
    _    -> exit({no_proxy,Node})
end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runs in the consumer process

init(Consumer, Node, Proxy) ->
  process_flag(trap_exit,true),
  prf:ticker_even(),
  case Proxy of
    no_proxy ->
      loop(#ld{node = Node, 
	       proxy = [],
	       consumer = Consumer, 
	       collectors = subscribe(Node,Consumer:collectors()),
	       consumer_data = Consumer:init(Node)});
    _ ->
      loop(#ld{node = Proxy,
	       proxy = {Node,Consumer:collectors()},
	       consumer = Consumer, 
	       collectors = subscribe(Proxy,[prfDog]),
	       consumer_data = Consumer:init(Node)})
  end.

loop(LD) ->
  receive
    {Stopper,stop} -> 
      ?log(stopping),
      do_stop(LD),
      Stopper ! stopped;
    {timeout, _, {tick}} when LD#ld.server == [] -> 
      prf:ticker_even(),
      subscribe(LD#ld.node,LD#ld.collectors),
      Cdata = (LD#ld.consumer):tick(LD#ld.consumer_data, []),
      ?LOOP(LD#ld{consumer_data = Cdata});
    {timeout, _, {tick}} -> 
      prf:ticker_even(),
      {Data,NLD} = get_data(LD),
      Cdata = (NLD#ld.consumer):tick(NLD#ld.consumer_data, de_proxy(LD,Data)),
      ?LOOP(NLD#ld{consumer_data = Cdata});
    {'EXIT',Pid,Reason} when Pid == LD#ld.server ->
      ?log({lost_target, Reason}),
      ?LOOP(LD#ld{server=[]});
    {'EXIT',Pid,normal} ->
      do_stop(LD),
      exit({got_EXIT, Pid, normal});
    {'EXIT',Pid,Reason} ->
      do_stop(LD),
      exit({got_EXIT, Pid, Reason});
    {subscribe, {ok, Pid}}  ->
      link(Pid),
      [Pid ! {config,CollData} || CollData <- LD#ld.config],
      ?LOOP(LD#ld{server = Pid,config=[]});
    {subscribe, {failed, R}} ->
      ?log({subscribe, {failed, R}}),
      ?LOOP(LD);
    {config,{consumer,Data}} ->
      Cdata = (LD#ld.consumer):config(LD#ld.consumer_data, Data),
      ?LOOP(LD#ld{consumer_data = Cdata});
    {config,CollData} ->
      ?LOOP(maybe_conf(CollData, LD))
  end.

de_proxy(LD,Data) ->
  case LD#ld.proxy of
    [] -> Data;
    {Node,Collectors} -> de_proxy(Data,Node,Collectors)
  end.

de_proxy([{prfDog,DogData}|_],Node,Collectors) ->
  try Trigger = trigger(Collectors),
      CollectorDatas = orddict:fetch({Node,Trigger},DogData),
      [C || C={Coll,_} <- CollectorDatas, lists:member(Coll,Collectors)]
  catch _:_ -> 
      []
  end.

trigger(Collectors) -> 
  case lists:member(prfSys,Collectors) of 
    true -> ticker
  end.

maybe_conf(CollData, LD) ->
  case LD#ld.proxy == [] of
    true -> do_config(CollData, LD);
    false-> ?log({no_config,running_with_proxy}),LD
  end.

do_config(CollData, LD) ->
  case LD#ld.server of
    [] -> LD#ld{config=[CollData|LD#ld.config]};
    Pid-> Pid ! {config,CollData},LD
  end.

do_stop(LD) ->
  prfTarg:unsubscribe(LD#ld.node, self()),
  (LD#ld.consumer):terminate(LD#ld.consumer_data).

get_data(LD) -> 
  case {get_datas(LD#ld.node), LD#ld.data} of
    {[],[]}		-> {[],LD};
    {[],[Data]}		-> {Data,LD#ld{data=[]}};
    {[Data],_}		-> {Data,LD#ld{data=[]}};
    {[Data,D2|_],_}	-> {Data,LD#ld{data=[D2]}}
  end.

get_datas(Node) -> 
  receive {{data, Node}, Data} -> [Data|get_datas(Node)]
  after 0 -> []
  end.

subscribe(Node, Collectors) ->
  Self = self(),
  spawn(fun() -> 
            %% this runs in its own process since it can block 
            %% nettick_time seconds (if the target is hung)
            try prfTarg:subscribe(Node, Self, Collectors) of
              {Pid,Tick} -> 
		maybe_change_ticktime(Tick),
                Self ! {subscribe, {ok, Pid}}
            catch
              _:R -> Self ! {subscribe, {failed,R}}
            end 
        end),
  Collectors.

maybe_change_ticktime(ignored) -> ok;
maybe_change_ticktime(Tick) -> net_kernel:set_net_ticktime(Tick).
