%%%-------------------------------------------------------------------
%%% File    : prfDtop.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created : 16 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(dtopConsumer).

-export([init/1, terminate/1, tick/2, collectors/0, config/2]).

-record(cld, {sort=cpu, node, prfSys, cpu, reds = 0, dreds, items=19}).

collectors() -> [prfPrc,prfSys].
init(Node) -> #cld{node = Node}.
terminate(_LD) -> ok.

config(LD, {sort,Sort}) when Sort==cpu;Sort==mem;Sort==msgq->LD#cld{sort=Sort};
config(LD,_) -> LD.
    
tick(LD,Data) ->
    case Data of
	[] -> LD;
	[{prfPrc,PrfPrc},{prfSys,PrfSys}] -> 
	    print(update_ld(LD,PrfSys),PrfSys,PrfPrc)
    end.

print(LD,PrfSys,{PrfPrcSys,PrfPrcRed,PrfPrcMem,PrfPrcMsg}) ->
    print_del(),
    print_sys(LD, PrfPrcSys++PrfSys),
    io:fwrite("~n",[]),
    print_tags(),
    print_procs(LD, PrfPrcRed,PrfPrcMem,PrfPrcMsg),
    LD.

update_ld(LD, PrfSys) ->
    Reds = wrap(0, catch element(1,lks(reductions, PrfSys))),
    Dreds = Reds-LD#cld.reds,
    OldSD = LD#cld.prfSys,
    Cpu = wrap(0,catch dv(rt(PrfSys)-rt(OldSD), wc(PrfSys)-wc(OldSD))),
    LD#cld{prfSys=PrfSys,cpu=round(100*Cpu), dreds=Dreds, reds=Reds}.

rt(Data) -> element(1,lks(runtime, Data)).
wc(Data) -> element(1,lks(wall_clock, Data)).
wrap(Val, {'EXIT',_}) -> Val;
wrap(_, Val) -> Val.

-define(SYSFORMAT, 
	"~-20s size ~8sM, cpu%~4s, procs~7s, "
	"runq~4s  ~2.2.0w:~2.2.0w:~2.2.0w~n"
	"memory[kB]:  proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n").
print_sys(LD, Sys) ->
    io:fwrite(?SYSFORMAT, wrap(sysI(), catch sysI(LD, Sys))).

sysI() -> [[],[],[],[],[],0,0,0,[],[],[],[],[]].

sysI(LD, Sys) ->
    {_, {H,M,S}} = calendar:now_to_local_time(lks(now, Sys)),
    [to_list(lks(node, Sys)),
     io_lib:fwrite("~w(~w)", [round(lks(beamsize, Sys)/1048576),
 			      round(lks(total, Sys)/1048576)]),
     to_list(LD#cld.cpu),
     to_list(lks(process_count,Sys)),
     to_list(lks(run_queue, Sys)),
     H, M, S,
     to_list(round(lks(processes, Sys)/1024)),
     to_list(round(lks(atom, Sys)/1024)),
     to_list(round(lks(binary, Sys)/1024)),
     to_list(round(lks(code, Sys)/1024)),
     to_list(round(lks(ets, Sys)/1024))].

print_del() -> io:fwrite("~s~n", [lists:duplicate(79, $-)]).

-define(FORMAT, "~-11s ~-32s ~-19s~5s~6s~4s~n").
-define(TAGS, ["pid","name","current","msgq","mem","cpu"]).
print_tags() -> io:fwrite(?FORMAT, ?TAGS).

print_procs(LD = #cld{sort=cpu,items=Items},PrfPrcRed,PrfPrcMem,_) -> 
    print_procs(LD, pad(PrfPrcRed,PrfPrcMem,Items),Items);
print_procs(LD = #cld{sort=msgq,items=Items},_,PrfPrcMem,PrfPrcMsg) ->
    print_procs(LD, pad(PrfPrcMsg,PrfPrcMem,Items),Items);
print_procs(LD = #cld{sort=mem,items=Items},_,PrfPrcMem,_) -> 
    print_procs(LD, PrfPrcMem,Items).

pad(Prc,PrcMem,Items) ->
    case length(Prc) == Items of
	true -> Prc;
	false -> pad(Prc,PrcMem,[],Items)
    end.

pad(_,_,_,0) -> [];
pad([Prc|Prcs],PrcMem,Pids,N) -> [Prc|pad(Prcs,PrcMem,[lks(pid,Prc)|Pids],N-1)];
pad([],[PrcMem|PrcMems],Pids,N) -> 
    case lists:member(lks(pid,PrcMem),Pids) of
	true -> pad([],PrcMems,Pids,N);
	false-> [PrcMem|pad([],PrcMems,Pids,N-1)]
    end;
pad([],[],_,_) -> [].
	    

print_procs(LD, PrfPrc,Items) -> 
    lists:foreach(fun(PP) -> procsI(LD,PP) end, PrfPrc),
    io:fwrite("~s",[lists:duplicate(Items-length(PrfPrc),10)]).

procsI(#cld{cpu = Cpu, dreds = Dreds}, PP) ->
    io:fwrite(?FORMAT, [pidf(to_list(lks(pid,PP))),
			funf(reg(PP)), 
			funf(lks(current_function, PP)), 
			to_list(lks(message_queue_len, PP)),
			to_list(round(lks(memory,PP)/1024)), 
			to_list(cpu(lks(diff_reds,PP), Dreds, Cpu))]).

cpu(Dred, Dreds, Cpu) -> round(dv(Cpu*Dred, Dreds)).

reg(PP) ->    
    case lks(registered_name, PP) of
	[] -> lks(initial_call, PP);
	Val -> Val
    end.

pidf(Pid) -> 
    {match,B,L} = regexp:match(Pid,"<[0-9]*"),
    [$<,$0|string:substr(Pid,B+L)].

funf({M, F, A}) -> "("++to_list(M)++":"++to_list(F)++"/"++to_list(A)++")";
funf(Term) -> io_lib:fwrite("~p", [Term]).

to_list(A) when pid(A) -> pid_to_list(A);
to_list(A) when atom(A) -> atom_to_list(A);
to_list(A) when float(A) -> float_to_list(A);
to_list(A) when tuple(A) -> tuple_to_list(A);
to_list(A) when integer(A) -> integer_to_list(A).

dv(_, 0) -> 0;
dv(A, B) -> A/B.

lks(Tag, [{Tag,Val}|_]) -> Val;
lks(Tag, [_|List]) -> lks(Tag, List).
