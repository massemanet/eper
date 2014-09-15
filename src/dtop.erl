%%%-------------------------------------------------------------------
%%% File    : dtop.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Created :  5 Sep 2005
%%% Description : top-like client for prf
%%%-------------------------------------------------------------------
-module(dtop).
-author('Mats Cronqvist').

-export([start/0,start/1,start/2,
         stop/0,
         sort/1,file/1,set_max_procs/1]).

start() -> start(node()).

start([Node,Proxy]) -> start(Node,Proxy);
start([Node]) -> start([Node,no_proxy]);
start(Node) -> start([Node]).

start(Node,Proxy) when is_atom(Node),is_atom(Proxy) ->
  prf:start(dtop,Node,dtopConsumer,Proxy).

stop() -> prf:stop(dtop).

sort(Sort) -> prf:config(dtop,consumer,{sort,Sort}).

file(Filename) ->
  try {ok,FD} = file:open(Filename,[write]),
      prf:config(dtop,consumer,{fd,FD}),
      prf:config(dtop,consumer,{items,no_pad})
  catch _:R -> {error,{R,Filename}}
  end.

set_max_procs(Max) when is_integer(Max) ->
  prf:config(dtop,prfPrc,{max_procs,Max}).
