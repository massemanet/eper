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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfPrc].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
tick(LD, Data) -> io:fwrite("** ~w **~n~w~n~p~n",[?MODULE,LD,Data]), LD.
config(LD,_Data) -> ?log({loopdata,LD}), LD.
