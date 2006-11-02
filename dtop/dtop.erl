%%%-------------------------------------------------------------------
%%% File    : dtop.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created :  5 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(dtop).

-export([start/1,stop/0,sort/1]).

start(Node) when atom(Node) -> start([Node]);
start([Node]) -> prfHost:start(dtop,Node,prfDtop).

stop() -> prfHost:stop(dtop).

sort(Sort) -> dtop ! {config,{sort,Sort}}.
