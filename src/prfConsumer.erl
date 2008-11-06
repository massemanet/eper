%%%-------------------------------------------------------------------
%%% File    : prfConsumer.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : an example consumer
%%%             : try e.g. 
%%              : prf:start(foo,node(),prfConsumer).
%%              : prf:stop(foo).
%%% Created : 17 May 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(prfConsumer).

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node}).

-include("log.hrl").

%% example usage;
%% watchdog:start(),prf:start(prf1,{watchdog,node()},prfConsumer).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfPrc].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
config(LD,_Data) -> ?log({loopdata,LD}), LD.
tick(LD, [Data|_]) -> 
  ?log(digger([watchdog,node,now,user],Data)),
  LD.
%%io:fwrite("** ~w **~n~w~n~p~n",[?MODULE,LD,Data]), LD.

digger(Tags,Data) ->
  try [{T,dig([T],Data)} || T <- Tags]
  catch _:_ -> empty
  end.

dig([T|Tags],Data) -> dig(Tags,lks(T,Data));
dig([],Data) -> Data.

lks(Tag,List) -> 
  try {value,{Tag,Val}} = lists:keysearch(Tag,1,List), Val
  catch _:_ -> throw({no_such_key,Tag,List})
  end.
