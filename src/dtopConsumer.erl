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
  lwrite(FD,"~n",[]),
  print_tags(FD),
  print_procs(FD,Items,PrfSys,which_sort(Sort,PrfPrc)).

print_sys(FD,Sys) ->
  lwrite(FD,"~s~n",[sys_str(Sys)]),
  lwrite(FD,memf(),memi(Sys)).

memf() -> "memory:      proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n".

memi(Sys) ->
  try [prf:human(lks(T,Sys)) || T <- [processes,atom,binary,code,ets]]
  catch _:_ -> ["","","","",""]
  end.

sys_str(Sys) ->
  {_, Time} = calendar:now_to_local_time(lks(now, Sys)),
  H         = pad(element(1,Time),2,$0,left),
  M         = pad(element(2,Time),2,$0,left),
  S         = pad(element(3,Time),2,$0,left),
  Node      = to_list(lks(node, Sys)),
  MEMbeam   = prf:human(lks(beam_vss,Sys,0)),
  MEM       = prf:human(lks(total,Sys)),
  CPUbeam   = to_list(100*(lks(beam_user,Sys,0)+lks(beam_kernel,Sys,0))),
  CPU       = to_list(100*(lks(user,Sys,0)+lks(kernel,Sys,0))),
  Procs     = prf:human(lks(procs,Sys)),
  RunQ      = prf:human(lks(run_queue, Sys)),

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
  lwrite(FD,"~s~n", [lists:duplicate(79, $-)]).

format() -> "~-14s ~-28s ~-17s~7s~7s~4s~n".

tags() -> ["pid","name","current","msgq","mem","cpu"].

print_tags(FD) ->
  lwrite(FD,format(),tags()).

which_sort( cpu,PrfPrc) -> expand(lks(dreds,PrfPrc),lks(info,PrfPrc));
which_sort(msgq,PrfPrc) -> expand(lks( msgq,PrfPrc),lks(info,PrfPrc));
which_sort(dmem,PrfPrc) -> expand(lks( dmem,PrfPrc),lks(info,PrfPrc));
which_sort( mem,PrfPrc) -> expand(lks(  mem,PrfPrc),lks(info,PrfPrc)).

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
  CPU =
    case lks(beam_user,Sys,1)+lks(beam_kernel,Sys,0) of
      0.0 -> 1;
      C -> C
    end,
  case lks(reductions,Sys) of
    0    -> 0;
    Reds -> 100*CPU/Reds
  end.

procsI(FD,PP,CpuPerRed) ->
  try
    lwrite(FD,
           format(),
           [pidf(to_list(lks(pid,PP))),
            funf(reg(PP)),
            funf(lks(current_function, PP)),
            prf:human(lks(message_queue_len, PP)),
            prf:human(lks(memory,PP)),
            to_list(lks(dreductions,PP)*CpuPerRed)])
  catch _:_ ->
      lwrite(FD,"~n",[])
  end.

reg(PP) ->
  case lks(registered_name, PP) of
    [] -> lks(initial_call, PP);
    Val -> Val
  end.

pidf(Pid) ->
  [_,A,B] = string:tokens(Pid,"."),
  lists:append(["<0.",A,".",B]).

funf({M, F, A}) -> to_list(M)++":"++to_list(F)++"/"++to_list(A);
funf(Term) -> io_lib:fwrite("~p", [Term]).

to_list(A) when is_list(A) -> A;
to_list(A) when is_pid(A) -> pid_to_list(A);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_float(A) -> to_list(round(A));
to_list(A) when is_tuple(A) -> tuple_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A).

lks(Tag,TVs,Def) ->
  try lks(Tag,TVs)
  catch {not_found, _} -> Def
  end.

lks(Tag, [])            -> throw({not_found, Tag});
lks(Tag, [{Tag,Val}|_]) -> Val;
lks(Tag, [_|List])      -> lks(Tag, List).

lwrite({Tab,LineNo},Format,As) when is_atom(Tab) ->
  L = ets:update_counter(Tab,LineNo,1),
  ets:insert(Tab,{L,flat(Format,As)});
lwrite(FD,Format,As) ->
  io:fwrite(FD,Format,As).

flat(F,A) ->
  lists:flatten(io_lib:fwrite(F,A)).
