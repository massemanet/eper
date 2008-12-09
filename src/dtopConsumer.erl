%%%-------------------------------------------------------------------
%%% File    : prfDtop.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created : 16 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(dtopConsumer).

-export([init/1, terminate/1, tick/2, collectors/0, config/2]).

-record(cld, {sort=cpu, items=19}).

collectors() -> [prfPrc,prfSys].
init(_Node) -> #cld{}.
terminate(_LD) -> ok.

config(LD, {sort,S}) when S==cpu; S==mem; S==msgq -> LD#cld{sort=S};
config(LD,_) -> LD.

tick(LD,Data) ->
  case Data of
    [{prfPrc,PrfPrc},{prfSys,PrfSys}] -> print(LD,PrfSys,PrfPrc), LD;
    _ -> LD
  end.

print(#cld{sort=Sort,items=Items},PrfSys,PrfPrc) ->
  print_del(),
  print_sys(PrfSys),
  io:fwrite("~n",[]),
  print_tags(),
  print_procs(Items,PrfSys,which_sort(Sort,PrfPrc)).

-define(SYSFORMAT, 
	"~-20s size ~8sM, cpu%~4s, procs~7s, "
	"runq~4s  ~2.2.0w:~2.2.0w:~2.2.0w~n"
	"memory[kB]:  proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n").
-define(SYSEMPTY, ["","","","","",0,0,0,"","","","",""]).

print_sys(Sys) ->
  io:fwrite("~s~n",[sys_str(Sys)]),
  io:fwrite(memf(),memi(Sys)).
memf() -> "memory[kB]:  proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n".

sys_str(Sys) ->
  {_, Time} = calendar:now_to_local_time(lks(now, Sys)),
  H		= pad(element(1,Time),2,$0,left),
  M		= pad(element(2,Time),2,$0,left),
  S		= pad(element(3,Time),2,$0,left),
  Node		= to_list(lks(node, Sys)),
  MEMbeam	= to_list(round(lks(beam_vss, Sys)/1048576)),
  MEM		= to_list(round(lks(total, Sys)/1048576)),
  CPUbeam	= to_list(100*(lks(beam_user,Sys)+lks(beam_kernel,Sys))),
  CPU    	= to_list(100*(lks(user,Sys)+lks(kernel,Sys))),
  Procs		= to_list(lks(procs,Sys)),
  RunQ		= to_list(lks(run_queue, Sys)),

  SYS = lists:sublist(lists:append(["size: "    ,MEM,
				    "("         ,MEMbeam,
				    ")M, cpu%: ",CPUbeam,
				    "("         ,CPU,
				    "), procs: ",Procs,
				    ", runq: "  ,RunQ,
				    ", ",H,":",M,":",S]),79),
  pad(Node,79-length(SYS),$ , right)++SYS.

pad(Item,Len,Pad,LeftRight) ->
  I = to_list(Item),
  case length(I) of
    L when L=:=Len -> I;
    L when L<Len -> case LeftRight of 
		      left -> lists:duplicate(Len-L,Pad)++I;
		      right-> I++lists:duplicate(Len-L,Pad)
		    end;
    _ -> lists:sublist(I,Len)
  end.

memi(Sys) ->
  try [to_list(lks(T,Sys)/1024) || T <- [processes,atom,binary,code,ets]]
  catch _:_ -> ["","","","",""]
  end.

print_del() -> io:fwrite("~s~n", [lists:duplicate(79, $-)]).

-define(FORMAT, "~-11s ~-32s ~-19s~5s~6s~4s~n").
-define(TAGS, ["pid","name","current","msgq","mem","cpu"]).
print_tags() -> io:fwrite(?FORMAT, ?TAGS).

which_sort( cpu,PrfPrc) -> expand(lks(dreds,PrfPrc),lks(info,PrfPrc));
which_sort(msgq,PrfPrc) -> expand(lks(msgq,PrfPrc),lks(info,PrfPrc));
which_sort(dmem,PrfPrc) -> expand(lks(dmem,PrfPrc),lks(info,PrfPrc));
which_sort( mem,PrfPrc) -> expand(lks(mem,PrfPrc),lks(info,PrfPrc)).

expand(Pids,Infos) -> 
  lists:reverse([[{pid,Pid}|lks(Pid,Infos)] || Pid <- Pids]).

print_procs(Items,PrfSys,Prcs) -> 
  CpuPerRed = cpu_per_red(PrfSys),
  [procsI(P,CpuPerRed) || P <- resize(Items,Prcs)].

resize(Items,Prcs) ->
  case Items < length(Prcs) of
    true -> lists:sublist(Prcs,Items);
    false-> Prcs++lists:duplicate(Items-length(Prcs),[])
  end.

cpu_per_red(Sys) ->
  case lks(reductions,Sys) of
    0 -> 0;
    Reds -> 100*(lks(beam_user,Sys)+lks(beam_kernel,Sys))/Reds
  end.

procsI([],_) -> io:fwrite("~n",[]);
procsI(PP,CpuPerRed) ->
  io:fwrite(?FORMAT, [pidf(to_list(lks(pid,PP))),
		      funf(reg(PP)), 
		      funf(lks(current_function, PP)), 
		      to_list(lks(message_queue_len, PP)),
		      to_list(lks(memory,PP)/1024), 
		      to_list(lks(dreductions,PP)*CpuPerRed)]).

reg(PP) ->    
  case lks(registered_name, PP) of
    [] -> lks(initial_call, PP);
    Val -> Val
  end.

pidf(Pid) -> 
  {match,B,L} = regexp:match(Pid,"<[0-9]*"),
  [$<,$0|string:substr(Pid,B+L)].

funf({M, F, A}) -> to_list(M)++":"++to_list(F)++"/"++to_list(A);
funf(Term) -> io_lib:fwrite("~p", [Term]).

to_list(A) when is_list(A) -> A;
to_list(A) when is_pid(A) -> pid_to_list(A);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_float(A) -> to_list(round(A));
to_list(A) when is_tuple(A) -> tuple_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A).

lks(Tag, [{Tag,Val}|_]) -> Val;
lks(Tag, [_|List]) -> lks(Tag, List).
