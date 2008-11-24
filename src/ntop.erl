%%%----------------------------------------------------------------
%%% File        : ntop.erl
%%% Author      : Mats Cronqvist <locmacr@mwlx084>
%%% Created     : 14 Feb 2007
%%% Description : 
%%%----------------------------------------------------------------
-module(ntop).
-author('Mats Cronqvist').

-export([start/1,stop/0]).

start(Node) when is_atom(Node) -> start([Node]);
start([Node]) -> prf:start(ntop,Node,ntopConsumer).

stop() -> prf:stop(ntop).
