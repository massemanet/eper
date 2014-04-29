%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  5 Feb 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('redbug_eunit').
-author('mats cronqvist').

-include_lib("eunit/include/eunit.hrl").

t_0_test() ->
  Filename = "redbug.txt",
  {_, _} = redbug:start("lists:sort", [{print_file, Filename}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  maybe_delete(Filename).

t_1_test() ->
  Filename = "redbug.txt",
  {_, _} = redbug:start("lists:sort->return", [{print_file, Filename}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual(<<"lists:sort/1->[1,2,3]">>,
               get_line_seg(Filename,4,2)),
  maybe_delete(Filename).

t_2_test() ->
  Filename = "redbug.txt",
  {_, _} = redbug:start("lists:sort->stack", [{print_file, Filename}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  Lines = lists:seq(3,lines(Filename)),
  ?assertEqual([true],
               lists:usort([is_mfa(get_line_seg(Filename,L,1))||L<-Lines])),
  maybe_delete(Filename).

t_3_test() ->
  Filename = "redbug.txt",
  Pid = spawn(fun()->receive P when is_pid(P)->P!ding;quit->ok end end),
  {_,_} = redbug:start(send,[{procs,Pid},{print_file, Filename}]),
  Pid ! self(),
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"ding">>,
              get_line_seg(Filename,2,4)),
  maybe_delete(Filename).

t_4_test() ->
  Filename = "redbug.txt",
  Pid = spawn(fun()->receive P when is_pid(P)->P!ding;quit->ok end end),
  {_,_} = redbug:start('receive',[{procs,Pid},{print_file, Filename}]),
  Pid ! pling,
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"pling">>,
              get_line_seg(Filename,2,3)),
  maybe_delete(Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace file utilities
maybe_show(Filename) ->
  [io:fwrite("~p~n",[read_file(Filename)]) || in_shell()].

lines(Filename) ->
  length(read_file(Filename)).

get_line_seg(Filename,Line,Seg) ->
  e(Seg,e(Line,read_file(Filename))).

read_file(Filename) ->
  {ok,C} = file:read_file(Filename),
  [[S||S<-re:split(L,"\s"),S=/=<<>>]||L<-re:split(C,"\n"),L=/=<<>>].

is_mfa(H) ->
  L = byte_size(H),
  {match,[{0,L}]} =:= re:run(H,"[a-zA_Z0-9\'/:_-]*/[0-9]+").

maybe_delete(Filename) ->
  [file:delete(Filename) || not in_shell()].

in_shell() ->
  lists:member("shell:eval_loop/3",stack()).

stack() ->
  stack(self()).
stack(P) ->
  [string:strip(e(2,(string:tokens(L,"(+)")))) || L<- bt(P), $0 =:= hd(L)].
bt(P) ->
  string:tokens(binary_to_list(e(2,(process_info(P,backtrace)))),"\n").

e(N,L) when is_list(L) -> lists:nth(N,L);
e(N,T) when is_tuple(T)-> element(N,T).
