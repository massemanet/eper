%%%-------------------------------------------------------------------
%%% File    : prf.erl
%%% Author  : Mats Cronqvist <etxmacr@axdt212>
%%% Description : 
%%%
%%% Created : 11 Dec 2006 by Mats Cronqvist <etxmacr@axdt212>
%%%-------------------------------------------------------------------
-module(prf).

-export([start/3,start/4,stop/1,config/3]).

%% prf internal
-export([log/2,ticker_odd/0,ticker_even/0]).

-define(TICK,2000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the API
%%% Name - atom() the name of this session
%%% Node - atom() where to start the collectors
%%% Consumer - atom() the name of the consumer callback module

start(Name,Node,Consumer) -> start(Name,Node,Consumer,no_proxy).
start(Name,Node,Consumer,Proxy) -> prfHost:start(Name,Node,Consumer,Proxy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name - atom() the name of this session

stop(Name) -> prfHost:stop(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name - atom() the name of this session
%%% Type - atom() consumer|Collector
%%% Collector - atom() module name of collector
%%% Data - term() the consumer callbacks config function is called with this

config(Name,Type,Data) -> prfHost:config(Name,Type,Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% prf internal 
ticker_even()-> erlang:start_timer(incr(?TICK,0), self(),{tick}).
ticker_odd() -> erlang:start_timer(incr(?TICK,?TICK div 2), self(),{tick}).

incr(Tick,Offset) ->
    {_, Sec, Usec} = now(),
    Skew = Tick div 4,
    Tick+Skew-((round(Sec*1000+Usec/1000)-Offset+Skew) rem Tick).

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).
