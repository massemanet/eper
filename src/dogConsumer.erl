%% -*- erlang-indent-level: 2 -*-
%%% Created : 25 Nov 2008 by Mats Cronqvist <masse@kreditor.se>
-module(dogConsumer).
-author('Mats Cronqvist').

-export([init/1,terminate/1,tick/2,collectors/0,config/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(ld,{msg}).

collectors() -> [prfDog].

init(_Node) -> #ld{}.

terminate(LD) -> LD#ld.msg.

config(LD,_) -> LD.

tick(LD,Data) -> LD#ld{msg = Data}.
