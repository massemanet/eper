%%%-------------------------------------------------------------------
%%% File    : dtopConsumer.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Created : 16 Dec 2003
%%% Description : rewrite of dtop for prf
%%%-------------------------------------------------------------------
-module(dtopConsumer).
-author('Mats Cronqvist').

-export(
   [init/1
    , terminate/1
    , tick/2
    , collectors/0
    , config/2]).

-record(cld,
        {fd=standard_io,
         sort=cpu,
         items=19}).

collectors() -> [prfPrc,prfSys].
init(_Node) -> #cld{}.
terminate(_LD) -> ok.

config(LD,{sort,S}) when S==cpu; S==mem; S==msgq -> LD#cld{sort=S};
config(LD,{fd,FD})                               -> LD#cld{fd=FD};
config(LD,{items,Items})                         -> LD#cld{items=Items};
config(LD,_)                                     -> LD.

tick(LD,Data) ->
  case Data of
    [{prfPrc,PrfPrc},{prfSys,PrfSys}] -> print(LD,PrfSys,PrfPrc), LD;
    _ -> LD
  end.

print(#cld{fd=FD,sort=Sort,items=Items},PrfSys,PrfPrc) ->
  print_del(FD),
  print_sys(FD,PrfSys),
  io:fwrite(FD,"~n",[]),
  print_tags(FD),
  print_procs(FD,Items,PrfSys,which_sort(Sort,PrfPrc)).

print_sys(FD,Sys) ->
  io:fwrite(FD,"~s~n",[sys_str(Sys)]),
  io:fwrite(FD,memf(),memi(Sys)).

memf() -> "memory:      proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n".

memi(Sys) ->
  try [human(lks(T,Sys)) || T <- [processes,atom,binary,code,ets]]
  catch _:_ -> ["","","","",""]
  end.


sys_str(Sys) ->
  {_, Time} = calendar:now_to_local_time(lks(now, Sys)),
  H	    = pad(element(1,Time),2,$0,left),
  M	    = pad(element(2,Time),2,$0,left),
  S	    = pad(element(3,Time),2,$0,left),
  Node	    = to_list(lks(node, Sys)),
  MEMbeam   = human(lks(beam_vss,Sys,0)),
  MEM	    = human(lks(total,Sys)),
  CPUbeam   = to_list(100*(lks(beam_user,Sys,0)+lks(beam_kernel,Sys))),
  CPU       = to_list(100*(lks(user,Sys,0)+lks(kernel,Sys,0))),
  Procs	    = human(lks(procs,Sys)),
  RunQ	    = human(lks(run_queue, Sys)),

  SYS = lists:sublist(lists:append(["size: "    ,MEM,
				    "("         ,MEMbeam,
				    "), cpu%: " ,CPUbeam,
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

print_del(FD) ->
  io:fwrite(FD,"~s~n", [lists:duplicate(79, $-)]).

format() -> "~-13s ~-29s ~-17s~7s~7s~4s~n".

tags() -> ["pid","name","current","msgq","mem","cpu"].

print_tags(FD) ->
  io:fwrite(FD,format(),tags()).

which_sort( cpu,PrfPrc) -> expand(lks(dreds,PrfPrc),lks(info,PrfPrc));
which_sort(msgq,PrfPrc) -> expand(lks(msgq,PrfPrc),lks(info,PrfPrc));
which_sort(dmem,PrfPrc) -> expand(lks(dmem,PrfPrc),lks(info,PrfPrc));
which_sort( mem,PrfPrc) -> expand(lks(mem,PrfPrc),lks(info,PrfPrc)).

expand(Pids,Infos) -> 
  lists:reverse([[{pid,Pid}|lks(Pid,Infos)] || Pid <- Pids]).

print_procs(FD,Items,PrfSys,Prcs) -> 
  CpuPerRed = cpu_per_red(PrfSys),
  [procsI(FD,P,CpuPerRed) || P <- resize(Items,Prcs)].

resize(no_pad,Prcs) ->
  Prcs;
resize(Items,Prcs) ->
  case Items < length(Prcs) of
    true -> lists:sublist(Prcs,Items);
    false-> Prcs++lists:duplicate(Items-length(Prcs),[])
  end.

cpu_per_red(Sys) ->
  case lks(reductions,Sys) of
    0    -> 0;
    Reds -> 100*(lks(beam_user,Sys,1)+lks(beam_kernel,Sys,0))/Reds
  end.

procsI(FD,PP,CpuPerRed) ->
  try
    io:fwrite(FD,
              format(),
              [pidf(to_list(lks(pid,PP))),
               funf(reg(PP)), 
               funf(lks(current_function, PP)), 
               human(lks(message_queue_len, PP)),
               human(lks(memory,PP)), 
               to_list(lks(dreductions,PP)*CpuPerRed)])
  catch _:_ ->
      io:fwrite(FD,"~n",[])
  end.

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

human(0)->
  "0";
human(I)->
  case math:log10(I) of
    M when 15=<M -> human(M-15,"P");
    M when 12=<M -> human(M-12,"T");
    M when  9=<M -> human(M-9,"G");
    M when  6=<M -> human(M-6,"M");
    M when  3=<M -> human(M-3,"k");
    _            -> flat("~w",[I])
  end.

human(E,M) ->
  flat("~.1f~s",[math:pow(10,E),M]).

flat(Format,Args) -> 
  lists:flatten(io_lib:fwrite(Format,Args)).

to_list(A) when is_list(A) -> A;
to_list(A) when is_pid(A) -> pid_to_list(A);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_float(A) -> to_list(round(A));
to_list(A) when is_tuple(A) -> tuple_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A).

lks(Tag,TVs,Def) ->
  try lks(Tag,TVs) 
  catch not_found -> Def
  end.

lks(_, [])              -> throw(not_found);
lks(Tag, [{Tag,Val}|_]) -> Val;
lks(Tag, [_|List])      -> lks(Tag, List).
