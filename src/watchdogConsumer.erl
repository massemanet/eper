%% -*- erlang-indent-level: 2 -*-
%%% Created : 25 Nov 2008 by Mats Cronqvist <masse@kreditor.se>
-module(watchdogConsumer).
-author('Mats Cronqvist').

-export([init/1,terminate/1,tick/2,collectors/0,config/2]).
-record(cld,{node}).

-include("log.hrl").

%% example usage;
%% watchdog:start(),prf:start(w1,node(),watchdogConsumer,node()).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> watchdog.

init(Node) -> #cld{node = Node}.

terminate(_LD) -> ok.

config(LD,_Data) -> ?log({loopdata,LD}),LD.

tick(LD,Data) ->
  case orddict:filter(fun({_,_,Ev},_)->Ev=/=ticker end,Data) of
    [] -> ok;
    Intriguing -> io:fwrite("~p~n",[Intriguing])
  end,
  LD.
