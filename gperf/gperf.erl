-module(gperf).
-export([start/1]).

start([Node]) -> 				%from bash
    prfHost:start([gperf,Node,gperfConsumer]),halt();
start(Node) -> 					%from erlang shell
    prfHost:start(gperf,Node,gperfConsumer).
