%%%-------------------------------------------------------------------
%%% File    : prfClient.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 17 May 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(prfClient).

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node}).

collectors() -> [prfPrc].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
tick(LD, Data) -> io:fwrite("~w~n~p~n",[LD,Data]), LD.
config(LD,_Data) -> LD.
