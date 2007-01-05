%%%-------------------------------------------------------------------
%%% File    : prf.erl
%%% Author  : Mats Cronqvist <etxmacr@axdt212>
%%% Description : 
%%%
%%% Created : 11 Dec 2006 by Mats Cronqvist <etxmacr@axdt212>
%%%-------------------------------------------------------------------
-module(prf).

-export([start/3,stop/1]).

%% prf internal
-export([log/2,ticker_odd/0,ticker_even/0]).

-define(TICK,2000).

start(Name, Node, Consumer) -> prfHost:start(Name, Node, Consumer).

stop(Name) -> prfHost:stop(Name).

ticker_even()-> erlang:start_timer(incr(?TICK,0), self(),{tick}).
ticker_odd() -> erlang:start_timer(incr(?TICK,?TICK div 2), self(),{tick}).

incr(Tick,Offset) ->
    {_, Sec, Usec} = now(),
    Skew = Tick div 4,
    Tick+Skew-((round(Sec*1000+Usec/1000)-Offset+Skew) rem Tick).

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).
