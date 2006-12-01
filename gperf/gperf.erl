-module(gperf).
-export([ni/1,start/0,log/2]).
-define(LOG(T), gperf:log(process_info(self()),T)).

ni(['']) -> gperfGtk:init();
ni([Node]) -> ?LOG(Node),self() ! {node,Node}, gperfGtk:init().

start() -> gperfGtk:start().

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).
