%% -*- erlang-indent-level: 2 -*-
%%% Created : 25 Nov 2008 by Mats Cronqvist <masse@kreditor.se>
-module(dogConsumer).
-author('Mats Cronqvist').

-export([init/1,terminate/1,tick/2,collectors/0,config/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(ld,{data=[]}).

collectors()  -> [prfDog].

init(_Node)   -> #ld{}.

terminate(LD) -> LD#ld.data.

config(LD,_)  -> LD.

tick(LD,Data) -> LD#ld{data = Data ++ LD#ld.data}.
