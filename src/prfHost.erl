%%%-------------------------------------------------------------------
%%% File    : prfHost.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  2 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfHost).

-export([start/3, stop/1,config/3]).
-export([init/2,loop/1]).				%internal

-record(ld, {node, server=[], collectors, config=[], 
             consumer, consumer_data, data=[]}).

-define(LOOP, ?MODULE:loop).
-define(LOG(T), prf:log(process_info(self()),T)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Node, Consumer) when atom(Name), atom(Node), atom(Consumer) -> 
  case whereis(Name) of
    undefined -> register(Name, spawn_link(fun()->init(Node,Consumer)end));
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

init(Node, Consumer) ->
  process_flag(trap_exit,true),
  prf:ticker_even(),
  loop(#ld{node = Node, 
           consumer = Consumer, 
           collectors = subscribe(Node,Consumer:collectors()),
           consumer_data = Consumer:init(Node)}).

loop(LD) ->
  receive
    {Stopper,stop} -> 
      ?LOG(stopping),
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
      Cdata = (NLD#ld.consumer):tick(NLD#ld.consumer_data, Data),
      ?LOOP(NLD#ld{consumer_data = Cdata});
    {'EXIT',Pid,Reason} when Pid == LD#ld.server ->
      ?LOG({lost_target, Reason}),
      ?LOOP(LD#ld{server=[]});
    {'EXIT',Pid,normal} ->
      do_stop(LD),
      exit({got_EXIT, Pid, normal});
    {'EXIT',Pid,Reason} ->
      do_stop(LD),
      exit({got_EXIT, Pid, Reason});
    {subscribe, {ok, Pid}}  ->
      [Pid ! {config,Data} || Data <- LD#ld.config],
      ?LOOP(LD#ld{server = Pid,config=[]});
    {subscribe, {failed, R}} ->
      ?LOG({subscribe, {failed, R}}),
      ?LOOP(LD);
    {config,{consumer, Data}} ->
      Cdata = (LD#ld.consumer):config(LD#ld.consumer_data, Data),
      ?LOOP(LD#ld{consumer_data = Cdata});
    {config,{collectors,Data}} ->
      case LD#ld.server of
        [] -> ?LOOP(LD#ld{config=[Data|LD#ld.config]});
        Pid-> Pid ! {config,Data}, ?LOOP(LD)
      end
  end.

do_stop(LD) ->
  prfTarg:unsubscribe(LD#ld.node, self()),
  (LD#ld.consumer):terminate(LD#ld.consumer_data).

get_data(LD) -> 
  case {get_datas(LD#ld.node), LD#ld.data} of
    {[],[]} -> {[],LD};
    {[],[Data]} -> {Data,LD#ld{data=[]}};
    {[Data],_} -> {Data,LD#ld{data=[]}};
    {[Data,D2|_],_} ->{Data,LD#ld{data=[D2]}}
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
                net_kernel:set_net_ticktime(Tick),
                Self ! {subscribe, {ok, Pid}}
            catch
              _:R -> Self ! {subscribe, {failed,R}}
            end 
        end),
  Collectors.
