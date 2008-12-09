%%%----------------------------------------------------------------
%%% File        : ntop.erl
%%% Author      : Mats Cronqvist <locmacr@mwlx084>
%%% Created     : 14 Feb 2007
%%% Description :  net-top client for prf
%%%----------------------------------------------------------------
-module(ntop).
-author('Mats Cronqvist').

-export([start/0,start/1,start/2,stop/0]).

start() -> start(node()).

start([Node,Proxy]) -> start(Node,Proxy);
start([Node]) -> start([Node,no_proxy]);
start(Node) -> start([Node]).

start(Node,Proxy) when is_atom(Node),is_atom(Proxy) ->
  prf:start(ntop,Node,ntopConsumer,Proxy).

stop() -> prf:stop(ntop).
