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
%% prf:start(prf1,node(),prfConsumer).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfSys].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
config(LD,_Data) -> ?log({loopdata,LD}), LD.

tick(LD, []) ->
  ?log(empty),
  LD;
tick(LD, [[]]) ->
  ?log(empty_d),
  LD;
tick(LD, [Data|_]) ->
  ?log({data,Data}),
  LD.
