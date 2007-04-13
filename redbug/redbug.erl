%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : redbg.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 24 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(redbug).

-export([help/0]).
-export([start/1]).
-export([start/3,start/4,start/5]).
-export([stop/0]).

-import(lists,[foldl/3,usort/1,reverse/1,foreach/2,flatten/1]).

-define(LOG(T), prf:log(process_info(self()),T)).

help() ->
  strs(["redbug:start(Time,Msgs,Trc[,Proc[,Targ]])",
        "Time: integer() [ms]",
        "Msgs: integer() [#]",
        "Trc: list('send'|'receive'|{M,F}|{M,F,RestrictedMatchSpecs})",
        "Proc: 'all'|pid()|atom(Regname)|{'pid',I2,I3}",
        "Targ: node()",
        "RestrictedMatchSpecs: list(RMS)",
        "RMS: 'stack'|'return'|tuple(ArgDescriptor)",
        "ArgDescriptor: '_'|literal()"]).

start([Node,Time,Msgs,Pat]) -> start([Node,Time,Msgs,Pat,"all"]);
start([Node,Time,Msgs,Pat,Proc]) ->
  try 
    self() ! {start,{term_stream,to_int(Time),to_int(Msgs),
                     to_term(Pat),to_atom(Proc),to_atom(Node)}},
    init(),
    erlang:halt()
  catch 
    C:R -> 
      io:fwrite("~p~n",[{C,R,erlang:get_stacktrace()}]),
      erlang:halt(1)
  end;
start(X) ->
  io:fwrite("bad args: ~p~n",[X]),
  erlang:halt(1).

start(Time,Msgs,Trc) -> start(Time,Msgs,Trc,all).

start(Time,Msgs,Trc,Proc) -> start(Time,Msgs,Trc,Proc,node()).

start(Time,Msgs,Trc,Proc,Targ)  -> 
  case whereis(redbug) of
    undefined -> 
      try 
        register(redbug, spawn(fun init/0)),
        redbug ! {start,{term_buffer,Time,Msgs,Trc,Proc,Targ}},
        ok
      catch C:R -> {oops,{C,R}}
      end;
    _ -> already_started
  end.

stop() ->
  case whereis(redbug) of
    undefined -> not_started;
    Pid -> Pid ! {stop,[]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  process_flag(trap_exit,true),
  receive 
    {start,{How,Time,Msgs,Trc,Proc,Targ}} -> 
      try 
        Conf = pack(How,Time,Msgs,Trc,Proc),
        prf:start(prf_redbug,Targ,redbugConsumer),
        prf:config(prf_redbug,collectors,{start,{self(),Conf}}),
        iloop()
      catch 
        C:R -> ?LOG([{C,R},{stack,erlang:get_stacktrace()}])
      end
  end.

iloop() ->
  receive
    {stop,Args} -> prf:config(prf_redbug,collectors,{stop,{self(),Args}});
    {prfTrc,{starting,TrcPid}}         -> loop(TrcPid);
    {prfTrc,{already_started,_TrcPid}} -> ?LOG(already_started);
    {'EXIT',Pid,R}                     -> ?LOG([{exited,Pid},{reason,R}]);
    {'EXIT',R}                         -> ?LOG([exited,{reason,R}]);
    X                                  -> ?LOG([{unknown_message,X}])
  end.

loop(TrcPid) ->
  receive
    {stop,Args} -> prf:config(prf_redbug,collectors,{stop,{self(),Args}});
    {prfTrc,{stopping,TrcPid,Args}}->  stop_msg(Args);
    {prfTrc,{not_started,TrcPid}}   -> ?LOG(not_started);
    {'EXIT',TrcPid,R}               -> ?LOG([tracer_died,{reason,R}]);
    {'EXIT',R}                      -> ?LOG([exited,{reason,R}]);
    X                               -> ?LOG([{unknown_message,X}])
  end.

stop_msg({timeout})                   -> io:fwrite("done: ~p~n",[timeout]);
stop_msg({consumer_died,{msg_count}}) -> io:fwrite("done: ~p~n",[msg_count]);
stop_msg(Args)                        -> ?LOG({stopping,Args}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,Pid,Count} | {term_stream,Pid,Count} |
%%%         {file,File,Size} | {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(How,Time,Msgs,Trc,Proc) ->
  {Flags,RTPs} = foldl(fun chk_trc/2,{[],[]},ass_list(Trc)),
  dict:from_list([{time,chk_time(Time)},
                  {flags,[call,timestamp|Flags]},
                  {rtps,RTPs},
                  {procs,chk_proc(Proc)},
                  {where,{How,ass_printer(),chk_msgs(Msgs)}}]).

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> exit({bad_msgs,X}).

chk_trc('send',{Flags,RTPs}) -> {['send'|Flags],RTPs};
chk_trc('receive',{Flags,RTPs}) -> {['receive'|Flags],RTPs};
chk_trc(RTP,{Flags,RTPs}) when is_tuple(RTP) -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(X,_) -> exit({bad_trc,X}).

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom)-> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1), is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> exit({bad_proc,X}).

chk_rtp({M,F}) when atom(M), atom(F), M/='_' -> {{M,F,'_'},[],[local]};
chk_rtp({M,F,MS}) when atom(M), atom(F), M/='_' -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X) -> exit({bad_rtp,X}).

ms(MS) -> foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack, [{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return, [{Head,Cond,Body}]) -> [{Head,Cond,[{return_trace}|Body]}];
msf(Head, [{_,Cond,Body}]) when tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> exit({bad_match_spec,X}).

ass_list(L) when is_list(L) -> usort(L);
ass_list(X) -> [X].

ass_printer() ->
  Self = self(),
  spawn_link(fun()->printi(Self) end).

strs([]) -> ok;
strs([H|T]) -> io:fwrite("~s~n",[H]),strs(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printi(Pid) ->
  erlang:monitor(process,Pid),
  printl().

printl() ->
  receive
    {'DOWN',_Ref,process,_Pid,_Reason} -> 
      io:fwrite("got DOWN from ~p~n",[node(_Pid)]);
    X -> outer(X), printl()
  end.

outer([]) -> ok;
outer([Msg|Msgs]) ->
  case Msg of
    {'call',{MFA,Bin},PI,TS} ->
      io:fwrite("~s <~p> ~p~n",[ts(TS),PI,MFA]),
      foreach(fun(L)->io:fwrite("  ~p~n",[L]) end, stak(Bin));
    {'retn',{MFA,Val},PI,TS} -> 
      io:fwrite("~s <~p> ~p -> ~p~n",[ts(TS),PI,MFA,Val]);
    {'send',{MSG,To},PI,TS} -> 
      io:fwrite("~s <~p> <~p> <<< ~p~n",[ts(TS),PI,To,MSG]);
    {'recv',MSG,PI,TS} -> 
      io:fwrite("~s <~p> <<< ~p~n",[ts(TS),PI,MSG]);
    MSG ->
      io:fwrite("~p~n", [MSG])
  end,
  outer(Msgs).

ts({H,M,S,_Us}) ->
  flatten(io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,S])).

%%% call stack handler
stak(Bin) ->
  foldl(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case reverse(I) of
    "..."++_ -> [truncated|Out];
    _ -> 
      case string:str(I, "Return addr") of
        0 -> 
          case string:str(I, "cp = ") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I, "erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_, C|_] = string:tokens(I,"()+"),
  case string:tokens(C,":/ ") of
    [M,F,A] ->
      try {to_atom(M),to_atom(F),to_int(A)}
      catch _:_ -> C
      end;
    ["unknown","function"] ->
      unknown_function
  end.

to_int(L) -> list_to_integer(L).
to_atom(L) -> list_to_atom(L).
to_term("_") -> '_';
to_term(Str) -> 
  {done, {ok, Toks, 1}, []} = erl_scan:tokens([], "["++Str++"]. ", 1),
  case erl_parse:parse_term(Toks) of
    {ok, [Term]} -> Term;
    {ok, L} when list(L) -> L
  end.
