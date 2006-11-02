%%%-------------------------------------------------------------------
%%% File    : prf.hrl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------

-define(ITEMS, 19).
-define(TICK, 2000).


-define(LOG(X), io:fwrite("~s - ~p:~p/~p - ~p~n", ?NOW++?FUNC++[X])).
-define(FUNC, ?T2L2(process_info(self(),current_function))).
-define(NOW, [io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w", ?DN(now()))]).
-define(DN(N), ?T2L2(calendar:now_to_datetime(N))++[element(3,N)]).
-define(T2L2(T), tuple_to_list(element(2,T))).

