%%%-------------------------------------------------------------------
%%% File    : prfHost.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  2 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfHost).

-export([start/1, start/3, stop/1]).
-export([init/3,loop/1]).				%internal

-record(ld, {node, server=[], collectors, consumer, consumer_data}).

-include("prf.hrl").

start([Name, Node, Consumer]) ->
    %%started from unix shell
    link(start(Name, Node, Consumer)), 
    receive {'EXIT',_,_} -> ok end.

start(Name, Node, Consumer) when atom(Name), atom(Node), atom(Consumer) -> 
    %%started from erlang shell
    case whereis(Name) of
	undefined -> spawn(?MODULE, init, [Name, Node, Consumer]);
	_Pid -> {already_started, Name}
    end.

stop(Name) when atom(Name) -> Name ! stop.

init(Name, Node, Consumer) ->
    process_flag(trap_exit,true),
    register(Name, self()),
    erlang:start_timer(incr(?TICK)+?TICK div 2, self(),{tick}),
    ?LOG(catch loop(#ld{node = Node, 
			consumer = Consumer, 
			collectors = subscribe(Node,Consumer:collectors()),
			consumer_data = Consumer:init(Node)})),
    exit(out).

loop(LD) ->
    receive
	stop -> 
	    ?LOG(stopping),
	    unsubscribe(LD#ld.node,LD#ld.collectors),
	    (LD#ld.consumer):terminate(LD#ld.consumer_data);
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
	    self() ! stop,
	    ?MODULE:loop(LD);
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
		  case catch prfTarg:subscribe(Node, Self, Collectors) of
		      {'EXIT',R} -> Self ! {subscribe, {failed,R}};
		      Pid ->  Self ! {subscribe, {ok, Pid}}
		  end 
	  end),
    Collectors.

incr(TCK) ->
    {_, _SEC_, _USEC_} = now(),
    TCK+1500-(round(_SEC_*1000+_USEC_/1000+500) rem TCK).
