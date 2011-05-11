%%%-------------------------------------------------------------------
%%% File    : prfHost.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description :
%%%
%%% Created :  2 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfHost).

-export([start/3,start/4,stop/1,config/3]).
-export([loop/1]).                              %internal

-record(ld, {node, server=[], collectors, config=[],
             proxy, consumer, consumer_data, data=[]}).

-define(LOOP, ?MODULE:loop).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runs in the shell

start(Name,Node,Consumer) -> start(Name,Node,Consumer,no_proxy).
start(Name,Node,Consumer,Proxy)
  when is_atom(Name),is_atom(Node), is_atom(Proxy) ->
  assert_proxy(Proxy),
  SpawnFun = fun()->init(Consumer,Node,Proxy) end,
  case whereis(Name) of
    undefined -> register(Name,Pid = spawner(SpawnFun)),Pid;
    Pid       -> Pid
  end.

spawner(F) ->
  case is_in_shell() of
    true -> spawn(F);
    false-> spawn_link(F)
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

is_in_shell() ->
  {_,{x,S}} = (catch erlang:error(x)),
  element(1,hd(lists:reverse(S))) == shell.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runs in the consumer process

init(Consumer, Node, Proxy) ->
  process_flag(trap_exit,true),
  prf:ticker_even(),
  Collectors = Consumer:collectors(),
  case Proxy of
    no_proxy when Collectors=:=watchdog ->
      exit(specify_proxy);
    no_proxy ->
      loop(#ld{node = Node,
               proxy = [],
               consumer = Consumer,
               collectors = subscribe(Node,Collectors),
               consumer_data = Consumer:init(Node)});
    _ ->
      loop(#ld{node = Proxy,
               proxy = {Node,Collectors},
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

de_proxy(_,[]) -> [];
de_proxy(LD,Data) ->
  case LD#ld.proxy of
    []              -> Data;
    {Node,watchdog} -> dog_data(Data,Node);
    {Node,Colls}    -> de_colls(Colls,dog_data(Data,Node))
  end.

de_colls(Colls,DogData) ->
  F0 = fun({_,Ev},_) -> Ev=:=ticker end,
  CD = orddict:filter(F0,DogData),
  F1 = fun(C,_) -> lists:member(C,Colls) end,
  orddict:filter(F1,CD).

dog_data([{prfDog,DogData}|_],Node) ->
  F = fun({N,_},_) -> N=:=Node end,
  orddict:filter(F, DogData).

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
    {[],[]}             -> {[],LD};
    {[],[Data]}         -> {Data,LD#ld{data=[]}};
    {[Data],_}          -> {Data,LD#ld{data=[]}};
    {[Data,D2|_],_}     -> {Data,LD#ld{data=[D2]}}
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
