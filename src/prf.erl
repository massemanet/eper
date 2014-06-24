%%%-------------------------------------------------------------------
%%% File    : prf.erl
%%% Author  : Mats Cronqvist <etxmacr@axdt212>
%%% Description :
%%%
%%% Created : 11 Dec 2006 by Mats Cronqvist <etxmacr@axdt212>
%%%-------------------------------------------------------------------
-module(prf).

-export([start/3,start/4,stop/1,config/3,state/1]).

%% prf internal
-export([log/2,ticker_odd/0,ticker_even/0]).
-export([human/1]).

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

state(Name) -> prfHost:state(Name).

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

%%make ints human readable
human(X) when not is_number(X) -> X;
human(I) when I < 0 -> "-"++human(-I);
human(I) when 0 < I ->
  case math:log10(I) of
    M when 15=<M -> human(M-15,"P");
    M when 12=<M -> human(M-12,"T");
    M when  9=<M -> human(M-9,"G");
    M when  6=<M -> human(M-6,"M");
    M when  3=<M -> human(M-3,"k");
    _            -> flat("~w",[I])
  end;
human(_) -> "0".

human(E,M) ->
  flat("~.1f~s",[math:pow(10,E),M]).

flat(Format,Args) ->
  lists:flatten(io_lib:fwrite(Format,Args)).
