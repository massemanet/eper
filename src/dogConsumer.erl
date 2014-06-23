%% -*- erlang-indent-level: 2 -*-
%%% Created : 25 Nov 2008 by Mats Cronqvist <masse@kreditor.se>
-module(dogConsumer).
-author('Mats Cronqvist').

-export([init/1,terminate/1,tick/2,collectors/0,config/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(ld,{node,
            dest_pid}).

collectors() -> [prfDog].

init(Node) ->
  #ld{node = Node}.

terminate(_LD) -> ok.

config(LD,{dest_pid,Pid}) -> LD#ld{dest_pid = Pid}.

tick(LD,Data) ->
  erlang:display({dogConsumer,Data}),
  LD.
