%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  5 Feb 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('redbug_eunit').
-author('mats cronqvist').

-include_lib("eunit/include/eunit.hrl").

t_0_test() ->
  Filename = "redbug.txt",
  {_, _} = redbug:start("lists:sort->return", [{print_file, Filename}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  ?assertEqual(trc_to_lines(Filename),
               [<<"{lists,sort,[[3,2,1]]}">>,
                <<"lists:sort/1->[1,2,3]">>]).

t_1_test() ->
  Filename = "redbug.txt",
  {_, _} = redbug:start("lists:sort->stack", [{print_file, Filename}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  [<<"{lists,sort,[[3,2,1]]}">>|Stack] = trc_to_lines(Filename),
  ?assert(is_mfas(Stack)).

%% trace file utilities
trc_to_lines(Filename) ->
  {ok,C} = file:read_file(Filename),
  file:delete(Filename),
  [list_to_binary(T)||[_,_|T]<-[re:split(L,"\s")||L<-re:split(C,"\n")]].

is_mfas([]) -> true;
is_mfas([H|T]) ->
  L = byte_size(H),
  {match,[{0,L}]} = re:run(H,"[a-zA_Z0-9\'/:_-]*/[0-9]+"),
  is_mfas(T).
