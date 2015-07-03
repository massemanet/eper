%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  5 Feb 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('redbug_eunit').
-author('mats cronqvist').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

t_0_test() ->
  Filename = "redbug.txt",
  {_,_} = redbug:start("lists:sort",[{print_file,Filename},print_msec]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual(4,
               length(re:split(get_line_seg(Filename,1,2),"[:.]"))),
  maybe_delete(Filename).

t_01_test() ->
  Filename = "redbug.txt",
  {_,_} = redbug:start("lists:sort",[{print_file,Filename},arity]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort/1">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual(3,
               length(re:split(get_line_seg(Filename,1,2),"[:.]"))),
  maybe_delete(Filename).

t_1_test() ->
  Filename = "redbug.txt",
  {_,_} = redbug:start("lists:sort->return",[{print_file,Filename},buffered]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(100),
  redbug:stop(),
  timer:sleep(100),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual([<<"lists:sort/1">>,<<"->">>,<<"[1,2,3]">>],
               get_line_seg(Filename,4,2,4)),
  maybe_delete(Filename).

t_2_test() ->
  Filename = "redbug.txt",
  {_,_} = redbug:start("lists:sort->stack",[{print_file,Filename}]),
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
  {_,_} = redbug:start(send,[{procs,Pid},{print_file,Filename}]),
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
  {_,_} = redbug:start('receive',[{procs,Pid},{print_file,Filename}]),
  Pid ! pling,
  timer:sleep(100),
  redbug:stop(),
  maybe_show(Filename),
  ?assertEqual(<<"pling">>,
              get_line_seg(Filename,2,3)),
  maybe_delete(Filename).

t_5_test() ->
  Filename = "redbug.txt",
  {_,_} = redbug:start("lists:sort->time",[{print_file,Filename},{time,999}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(1100),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual(<<"lists:sort/1">>,
               get_line_seg(Filename,3,6)),
  maybe_delete(Filename).

t_6_test() ->
  Filename = "redbug.txt",
  {_,_} = redbug:start("lists:sort->count",[{print_file,Filename},{time,999}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(1100),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual(<<"lists:sort/1">>,
               get_line_seg(Filename,3,4)),
  maybe_delete(Filename).

t_7_test() ->
  {_,Msgs} = redbug:start("erlang",[blocking,{time,999},arity]),
  ?assertEqual([{{erlang,monitor,2},<<>>},
                {{erlang,demonitor,1},<<>>}],
               [MFA || {_,MFA,_,_}<-Msgs]).

t_8_test() ->
  {_,_} = redbug:start("lists:sort->return",[{file,"foo"},{time,999}]),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(1100),
  {2,Msgs} = replay_trc:go("foo0.trc",fun(E,A)->[E]++A end,[]),
  ?assertEqual(sort,
               e(2,e(4,e(1,Msgs)))),
  ?assertEqual(sort,
               e(2,e(4,e(2,Msgs)))).

t_9_test() ->
  Filename = "redbug.txt",
  Options = [{print_file, Filename}, {time, 999}, {print_return, false}],
  {_,_} = redbug:start("lists:sort->return", Options),
  [1,2,3] = lists:sort([3,2,1]),
  timer:sleep(1100),
  maybe_show(Filename),
  ?assertEqual(<<"lists:sort([3,2,1])">>,
               get_line_seg(Filename,2,2)),
  ?assertEqual([<<"lists:sort/1">>,<<"->">>,<<"'...'">>],
               get_line_seg(Filename,4,2,4)),
  maybe_delete(Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace file utilities
maybe_show(Filename) ->
  [io:fwrite("~p~n",[read_file(Filename)]) || in_shell()].

lines(Filename) ->
  length(read_file(Filename)).

get_line_seg(Filename,Line,Seg) ->
  hd(get_line_seg(Filename,Line,Seg,Seg)).

get_line_seg(Filename,Line,SegF,SegL) ->
  [e(S,e(Line,read_file(Filename))) || S <- lists:seq(SegF,SegL)].

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
  [string:strip(e(2,(string:tokens(L,"(+)")))) || L<- bt(P),$0 =:= hd(L)].
bt(P) ->
  string:tokens(binary_to_list(e(2,(process_info(P,backtrace)))),"\n").

e(N,L) when is_list(L) -> lists:nth(N,L);
e(N,T) when is_tuple(T)-> element(N,T).

-endif.
