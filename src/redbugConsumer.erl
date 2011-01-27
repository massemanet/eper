%%%-------------------------------------------------------------------
%%% File    : redbugConsumer.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 23 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(redbugConsumer).

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node}).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfTrc].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.
tick(LD,_Data) -> LD.
config(LD,_Data) -> ?log({loopdata,LD}), LD.
