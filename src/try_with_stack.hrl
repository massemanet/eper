%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% work around the deprecation of erlang:get_stacktrace/0 in erlang 21
%% @end

-ifdef('USE_GET_STACKTRACE').
-define(try_with_stack(F),
        try {ok,F} catch __C:__R -> {__C,__R,erlang:get_stacktrace()} end).
-else.
-define(try_with_stack(F),
        try {ok,F} catch __C:__R:__S -> {__C,__R,__S} end).
-endif.
