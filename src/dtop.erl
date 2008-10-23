%%%-------------------------------------------------------------------
%%% File    : dtop.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created :  5 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(dtop).

-export([start/0,start/1,stop/0,sort/1]).

start()-> start(node()).
start(Node) when atom(Node) -> start([Node]);
start([Node]) -> prf:start(dtop,Node,dtopConsumer).

stop() -> prf:stop(dtop).

sort(Sort) -> prf:config(dtop,consumer,{sort,Sort}).
