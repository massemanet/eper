%%%-------------------------------------------------------------------
%%% File    : prfHost.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  2 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfHost).

-export([start/3, stop/1]).
-export([init/3,loop/1]).				%internal

-record(ld, {node, server=[], collectors, consumer, consumer_data}).

-include("prf.hrl").

start(Name, Node, Consumer) when atom(Name), atom(Node), atom(Consumer) -> 
    case whereis(Name) of
	undefined -> spawn_link(?MODULE, init, [Name, Node, Consumer]);
	Pid -> Pid
    end.

stop(Name) -> 
    case whereis(Name) of
        Pid when is_pid(Pid) -> Name ! {self(),stop}, receive stopped -> ok end;
        _ -> ok
    end.

init(Name, Node, Consumer) ->
    process_flag(trap_exit,true),
    register(Name, self()),
    erlang:start_timer(incr(?TICK)+?TICK div 2, self(),{tick}),
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
	    erlang:start_timer(incr(?TICK), self(),{tick}),
	    subscribe(LD#ld.node,LD#ld.collectors),
	    Cdata = (LD#ld.consumer):tick(LD#ld.consumer_data, []),
	    ?MODULE:loop(LD#ld{consumer_data = Cdata});
	{timeout, _, {tick}} -> 
	    erlang:start_timer(incr(?TICK), self(),{tick}),
	    Data = get_data(LD),
	    Cdata = (LD#ld.consumer):tick(LD#ld.consumer_data, Data),
	    ?MODULE:loop(LD#ld{consumer_data = Cdata});
	{'EXIT',Pid,Reason} when Pid == LD#ld.server ->
	    ?LOG({lost_target, Reason}),
	    ?MODULE:loop(LD#ld{server=[]});
	{'EXIT',Pid,Reason} ->
	    ?LOG({got_EXIT, Pid, Reason}),
            do_stop(LD),
            exit({got_EXIT, Pid, Reason});
	{subscribe, {ok, Pid}}  ->
	    ?LOG({subscribe, {ok, node(Pid)}}),
	    ?MODULE:loop(LD#ld{server = Pid});
	{subscribe, {failed, R}} ->
	    ?LOG({subscribe, {failed, R}}),
	    ?MODULE:loop(LD);
	{config, Data} ->
	    Cdata = (LD#ld.consumer):config(LD#ld.consumer_data, Data),
	    ?MODULE:loop(LD#ld{consumer_data = Cdata})
    end.

do_stop(LD) ->
    unsubscribe(LD#ld.node,LD#ld.collectors),
    (LD#ld.consumer):terminate(LD#ld.consumer_data).

get_data(LD) -> get_data(LD#ld.node, []).
get_data(Node, InData) ->
    receive 
	{{data, Node}, Data} -> get_data(Node, [Data|InData])
    after 0 -> InData
    end.

unsubscribe(Node, Collectors) ->
    prfTarg:unsubscribe(Node, self(), Collectors).

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

incr(TCK) ->
    {_, _SEC_, _USEC_} = now(),
    TCK+1500-(round(_SEC_*1000+_USEC_/1000+500) rem TCK).
