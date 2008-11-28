%%%-------------------------------------------------------------------
%%% File    : dtop.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created :  5 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(dtop).

-export([start/0,start/1,start/2,stop/0,sort/1]).

start() -> start(node()).

start([Node,Proxy]) -> start(Node,Proxy);
start([Node]) -> start([Node,no_proxy]);
start(Node) -> start([Node]).

start(Node,Proxy) when is_atom(Node),is_atom(Proxy) ->
  prf:start(dtop,Node,dtopConsumer,Proxy).

stop() -> prf:stop(dtop).

sort(Sort) -> prf:config(dtop,consumer,{sort,Sort}).
