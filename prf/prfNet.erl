%%%-------------------------------------------------------------------
%%% File    : prfNet.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : prf collector of net_kernel info
%%%
%%% Created : 18 Oct 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(prfNet).

-export([collect/1]).

-include("prf.hrl").

%%% returns {State, Data}
collect(State) -> {State, {?MODULE, data()}}.
data() ->
    case catch net_kernel:nodes_info() of
	{ok,L} -> L;
	_ -> []
    end.
