%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 23 Aug 2016 by masse <mats.cronqvist@gmail.com>

%% @doc
%% @end

-module('mtop').
-author('masse').
-export([start/0]).

start() ->
  register(mtop,self()),
  {State,_}=prfMnesia:collect(init),
  Opts = [{collectors,
           [held_locks,lock_queue,current_transactions,failed_transactions,
            committed_transactions,restarted_transactions,
            current_transactions_change,failed_transactions_change,
            committed_transactions_change,restarted_transactions_change]}],
  loop(prfMnesia:config(State,Opts)).


loop(State0) ->
  {State,{prfMnesia,Data}} = prfMnesia:collect(State0),
  {info,Participants,Coordinators} = mnesia_tm:get_info(1000),
  Trans = [{participants,length(Participants)},
           {coordinators,length(Coordinators)}],
  io:fwrite("~p~n",[Data++Trans]),
  receive
    quit -> exit(done)
  after
    2000 -> ok
  end,
  loop(State).


