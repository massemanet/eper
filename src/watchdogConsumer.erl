%% -*- erlang-indent-level: 2 -*-
%%% Created : 25 Nov 2008 by Mats Cronqvist <masse@kreditor.se>
-module(watchdogConsumer).
-author('Mats Cronqvist').

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node}).

-include("log.hrl").

%% example usage;
%% watchdog:start(),prf:start(w1,node(),watchdogConsumer,node()).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfSys,prfPrc].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
config(LD,_Data) -> ?log({loopdata,LD}), LD.

tick(LD,Data) ->
  do_tick(Data),
  LD.
  
do_tick([]) -> ok;
do_tick([[]]) -> ?log(double_empty);
do_tick(Data) -> 
  PrfSys = lks(prfSys,Data),
  io:fwrite("~w: ~p~n",[lks(node,PrfSys),
			calendar:now_to_local_time(lks(now,PrfSys))]).

lks(Tag,List) -> 
  try {value,{Tag,Val}} = lists:keysearch(Tag,1,List), Val
  catch _:_ -> throw({no_such_key,Tag,List})
  end.
