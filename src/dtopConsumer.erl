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
  try 
    io:fwrite(?SYSFORMAT, sysI(Sys))
  catch 
    _:_ -> io:fwrite(?SYSFORMAT, ?SYSEMPTY)
  end.

sysI(Sys) ->
  {_, {H,M,S}} = calendar:now_to_local_time(lks(now, Sys)),
  [to_list(lks(node, Sys)),
   io_lib:fwrite("~w(~w)", [round(lks(beam_vss, Sys)/1048576),
			    round(lks(total, Sys)/1048576)]),
   to_list((lks(beam_user,Sys)+lks(beam_kernel,Sys))),
   to_list(lks(procs,Sys)),
   to_list(lks(run_queue, Sys)),
   H, M, S,
   to_list(lks(processes, Sys)/1024),
   to_list(lks(atom, Sys)/1024),
   to_list(lks(binary, Sys)/1024),
   to_list(lks(code, Sys)/1024),
   to_list(lks(ets, Sys)/1024)].

print_del() -> io:fwrite("~s~n", [lists:duplicate(79, $-)]).

-define(FORMAT, "~-11s ~-32s ~-19s~5s~6s~4s~n").
-define(TAGS, ["pid","name","current","msgq","mem","cpu"]).
print_tags() -> io:fwrite(?FORMAT, ?TAGS).

which_sort( cpu,PrfPrc) -> lks(reds,PrfPrc);
which_sort(msgq,PrfPrc) -> lks(msg,PrfPrc);
which_sort( mem,PrfPrc) -> lks(mem,PrfPrc).

print_procs(Items,PrfSys,Prcs) -> 
  CpuPerRed = cpu_per_red(PrfSys),
  lists:foreach(fun(P) -> procsI(P,CpuPerRed) end, resize(Items,Prcs)).

resize(Items,Prcs) ->
  case Items < length(Prcs) of
    true -> lists:sublist(Prcs,Items);
    false-> Prcs++lists:duplicate(Items-length(Prcs),dummy)
  end.

cpu_per_red(Sys) ->
  case lks(reductions,Sys) of
    0 -> 0;
    Reds -> (lks(beam_user,Sys)+lks(beam_kernel,Sys))/Reds
  end.

procsI(PP,CpuPerRed) ->
  io:fwrite(?FORMAT, [pidf(to_list(lks(pid,PP))),
		      funf(reg(PP)), 
		      funf(lks(current_function, PP)), 
		      to_list(lks(message_queue_len, PP)),
		      to_list(lks(memory,PP)/1024), 
		      to_list(lks(reductions,PP)*CpuPerRed)]).

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
to_list(A) when float(A) -> to_list(round(A));
to_list(A) when tuple(A) -> tuple_to_list(A);
to_list(A) when integer(A) -> integer_to_list(A).

lks(Tag, [{Tag,Val}|_]) -> Val;
lks(Tag, [_|List]) -> lks(Tag, List).
