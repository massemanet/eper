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

%% sensible defaults;
%% Proc = all
%% Targ = node()
%% Buffered = no
%% PrintF = print_fun()
-record(cnf,{time,msgs,trc,
             proc=all,
	     targ=node(),
	     buffered=no,
	     printf=print_fun(),
	     max_queue=5000,
	     max_msg_size=50000}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_fun() -> fun(Str) -> io:fwrite("~s~n",[Str]) end.

help() ->
  F = print_fun(),
  foreach(F,["redbug:start(Time,Msgs,Trc[,Proc[,Targ]])",
             "Time: integer() [ms]",
             "Msgs: integer() [#]",
             "Trc: list('send'|'receive'|{M}|{M,F}|{M,RMSs}|{M,F,RMSs})",
             "Proc: 'all'|pid()|atom(Regname)|{'pid',I2,I3}",
             "Targ: node()",
             "RMSs: (restricted match specs): list(RMS)",
             "RMS: 'stack'|'return'|tuple(ArgDescriptor)",
             "ArgDescriptor: '_'|literal()"]).

start([Node,Time,Msgs,Trc]) -> start([Node,Time,Msgs,Trc,"all"]);
start([Node,Time,Msgs,Trc,Proc]) ->
  try 
    Cnf = #cnf{time=to_int(Time),
               msgs=to_int(Msgs),
               trc=to_term(Trc),
               proc=to_atom(Proc),
               targ=to_atom(Node)},
    self() ! {start,Cnf},
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

start(Time,Msgs,Trc) -> go(Time,Msgs,Trc,#cnf{}).

start(Time,Msgs,Trc,Proc) -> go(Time,Msgs,Trc,#cnf{proc=Proc}).

start(Time,Msgs,Trc,Proc,Targ)  -> go(Time,Msgs,Trc,#cnf{targ=Targ,proc=Proc}).

go(Time,Msgs,Trc,Cnf) ->
  case whereis(redbug) of
    undefined -> 
      try 
        register(redbug, spawn(fun init/0)),
        redbug ! {start,Cnf#cnf{time=Time,msgs=Msgs,trc=Trc}},
        ok
      catch C:R -> {oops,{C,R}}
      end;
    _ -> redbug_already_started
  end.

stop() ->
  case whereis(redbug) of
    undefined -> not_started;
    Pid -> Pid ! {stop,[]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the main redbug process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  process_flag(trap_exit,true),
  receive 
    {start,Cnf} -> 
      try 
        PrintPid = spawn_link(fun()->printi(Cnf#cnf.printf) end),
        Conf = pack(Cnf,PrintPid),
        prf:start(prf_redbug,Cnf#cnf.targ,redbugConsumer),
        prf:config(prf_redbug,collectors,{start,{self(),Conf}}),
	starting(PrintPid)
      catch 
        C:R -> ?LOG([{C,R},{stack,erlang:get_stacktrace()}])
      end
  end,
  exit(exiting).

starting(PrintPid) ->
  receive
    {stop,Args} -> prf:config(prf_redbug,collectors,{stop,{self(),Args}});
    {prfTrc,{starting,TrcPid,ConsPid}} -> running(TrcPid,ConsPid,PrintPid);
    {prfTrc,{already_started,_}}       -> ?LOG(already_started);
    {'EXIT',PrintPid,R}                -> ?LOG([printer_died,{reason,R}]);
    {'EXIT',R}                         -> ?LOG([exited,{reason,R}]);
    X                                  -> ?LOG([{unknown_message,X}])
  end.

running(TrcPid,ConsPid,PrintPid) ->
  PrintPid ! {trace_consumer,ConsPid},
  receive
    {stop,Args} -> prf:config(prf_redbug,collectors,{stop,{self(),Args}}),
		   stopping(PrintPid);
    {prfTrc,{stopping,_,_}}       -> stopping(PrintPid);
    {'EXIT',TrcPid,_}             -> stopping(PrintPid);
    {prfTrc,{not_started,TrcPid}} -> ?LOG(not_started);
    {'EXIT',PrintPid,_}           -> maybe_stopping(TrcPid);
    X                             -> ?LOG([{unknown_message,X}])
  end.

maybe_stopping(TrcPid) ->
  receive
    {prfTrc,{stopping,_,_}} -> ok;
    {'EXIT',TrcPid,_}       -> ok;
    X                       -> ?LOG({unknown_message,X})
  end.

stopping(PrintPid) ->
  receive
    {'EXIT',PrintPid,_} -> ok;
    X                   -> ?LOG([{unknown_message,X}])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,{Pid,Count,MaxQueue,MaxSize}} | 
%%%         {term_stream,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {file,File,Size} | 
%%%         {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(Cnf,PrintPid) ->
  {Flags,RTPs} = foldl(fun chk_trc/2,{[],[]},ass_list(Cnf#cnf.trc)),
  dict:from_list([{time,chk_time(Cnf#cnf.time)},
                  {flags,[call,timestamp|Flags]},
                  {rtps,RTPs},
                  {procs,chk_proc(Cnf#cnf.proc)},
                  {where,{chk_buffered(Cnf#cnf.buffered),
                          {PrintPid,
			   chk_msgs(Cnf#cnf.msgs),
			   Cnf#cnf.max_queue,
			   Cnf#cnf.max_msg_size}}}]).

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_buffered(yes) -> term_buffer;
chk_buffered(no) -> term_stream.

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom)-> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1), is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> exit({bad_proc,X}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> exit({bad_msgs,X}).

chk_trc('send',{Flags,RTPs}) -> {['send'|Flags],RTPs};
chk_trc('receive',{Flags,RTPs}) -> {['receive'|Flags],RTPs};
chk_trc(RTP,{Flags,RTPs}) when is_tuple(RTP) -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(X,_) -> exit({bad_trc,X}).

chk_rtp({M})                                    -> chk_rtp({M,'_',[]});
chk_rtp({M,F}) when atom(F)                     -> chk_rtp({M,F,[]});
chk_rtp({M,L}) when list(L)                     -> chk_rtp({M,'_',L});
chk_rtp({'_',_,_})                              -> exit(dont_wildcard_module);
chk_rtp({M,F,MS}) when atom(M),atom(F),list(MS) -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X)                                      -> exit({bad_rtp,X}).

ms(MS) -> foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack, [{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return, [{Head,Cond,Body}]) -> [{Head,Cond,[{return_trace}|Body]}];
msf(Head, [{_,Cond,Body}]) when tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> exit({bad_match_spec,X}).

ass_list(L) when is_list(L) -> usort(L);
ass_list(X) -> [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printi(PrintF) ->
  receive 
    {trace_consumer,TC} -> 
      erlang:monitor(process,TC),
      printl(PrintF)
  end.

printl(PrintF) ->
  receive
    {'DOWN',_,_,_Pid,R}-> io:fwrite("quitting: ~p~n",[R]);
    X                  -> outer(PrintF,X), printl(PrintF)
  end.

outer(_,[]) -> ok;
outer(PrintF,[Msg|Msgs]) ->
  case Msg of
    {'call',{MFA,Bin},PI,TS} ->
      PrintF(flat("~s <~p> ~p~n",[ts(TS),PI,MFA])),
      foreach(fun(L)->PrintF(flat("  ~p~n",[L])) end, stak(Bin));
    {'retn',{MFA,Val},PI,TS} -> 
      PrintF(flat("~s <~p> ~p -> ~p~n",[ts(TS),PI,MFA,Val]));
    {'send',{MSG,To},PI,TS} -> 
      PrintF(flat("~s <~p> <~p> <<< ~p~n",[ts(TS),PI,To,MSG]));
    {'recv',MSG,PI,TS} -> 
      PrintF(flat("~s <~p> <<< ~p~n",[ts(TS),PI,MSG]));
    MSG ->
      PrintF(flat("~p~n", [MSG]))
  end,
  outer(PrintF,Msgs).

ts({H,M,S,_Us}) -> flat("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,S]).

flat(Form,List) -> flatten(io_lib:fwrite(Form,List)).

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
