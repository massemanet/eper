%% -*- erlang-indent-level: 2 -*-
%%% Created : 25 Nov 2008 by Mats Cronqvist <masse@kreditor.se>
-module(watchdogConsumer).
-author('Mats Cronqvist').

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node}).

-include("log.hrl").

%% example usage;
%% watchdog:start(),prf:start(prf1,{watchdog,node()},prfConsumer).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfSys,prfPrc].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
config(LD,_Data) -> ?log({loopdata,LD}), LD.

tick(LD,Data) ->
  do_tick(Data),
  LD.
  
do_tick([]) -> ok;
do_tick([[]]) -> ok;
do_tick([[{watchdog,user}|Data]|_]) -> 
  ?log([{userData,Data}]);
do_tick([[{watchdog,ticker}|Data]|_]) -> 
  ?log([ticker|digger([node,now,user],Data)]);
do_tick([[{watchdog,sysMon}|Data]|_]) -> 
  ?log([sysMon|Data]);
do_tick([[{watchdog,Trigger}|Data]|_]) -> 
  ?log([{trigger,Trigger}|digger([node,now,user],Data)]).


digger(Tags,Data) ->
  try [{T,dig([T],Data)} || T <- Tags]
  catch _:_ -> no_data
  end.

dig([T|Tags],Data) -> dig(Tags,lks(T,Data));
dig([],Data) -> Data.

lks(Tag,List) -> 
  try {value,{Tag,Val}} = lists:keysearch(Tag,1,List), Val
  catch _:_ -> throw({no_such_key,Tag,List})
  end.
