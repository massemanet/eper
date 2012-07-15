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
% start from the unix shell
-export([unix/1]).
% start from erlang shell
-export([start/1,start/2,start/3,start/4,start/5]).
-export([stop/0]).

-import(lists,[foldl/3,usort/1,reverse/1,foreach/2,flatten/1]).

-include("log.hrl").

%-define(bla,erlang:display(process_info(self(),current_function))).

%% the redbug server data structure
%% most can be set in the input proplist
-record(cnf,{time         = 15000          % stop trace after this time [ms]
             , msgs         = 10           % stop trace after this # msgs [unit]
             , proc         = all          % list of procs (or 'all')
             , target       = node()       % target node
             , cookie       = ''           % target node cookie
             , buffered     = false        % output buffering
             , arity        = false        % arity instead of args
             , print_calls  = true         % print calls
             , print_file   = ""           % file to print to (standard_io)
             , print_msec   = false        % print milliseconds in timestamps?
             , print_depth  = 999999       % Limit for "~P" formatting depth
             , print_re     = ""           % regexp that must match to print
             , print_fun    = ''           % custom print handler
             , max_queue    = 5000         % max # of msgs before suicide
             , max_msg_size = 50000        % max message size before suicide
             , file         = ""           % file to write trace msgs to
             , file_size    = 1            % file size (per file [Mb])
             , file_count   = 8            % number of files in wrap log
             , debug        = false        % big error messages

             , trc          = []           % cannot be set by user
             , print_pid    = []           % cannot be set by user
             , trc_pid      = []           % cannot be set by user
             , cons_pid     = []           % cannot be set by user
            }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help() ->
  foreach(fun(S)->io:fwrite(standard_io,"~s~n",[S])end,
          ["redbug - the (sensibly) Restrictive Debugger"
           , ""
           , "  redbug:start(Trc) -> start(Trc,[])."
           , "  redbug:start(Trc,Opts)."
           , ""
           , "  redbug is a tool to interact with the Erlang trace facility."
           , "  It will instruct the Erlang VM to generate so called "
           , "  'trace messages' when certain events (such as a particular"
           , "  function being called) occur."
           , "  The trace messages are either printed (i.e. human readable)"
           , "  to a file or to the screen; or written to a trc file."
           , "  Using a trc file puts less stress on the system, but"
           , "  there is no way to count the messages (so the msgs opt"
           , "  is ignored), and the files can only be read by special tools"
           , "  (such as 'bread'). Printing and trc files cannot be combined."
           , "  By default (i.e. if the 'file' opt is not given), messages"
           , "  are printed."
           , ""
           , "Trc: list('send'|'receive'|string(RTP))"
           , "RTP:  restricted trace pattern"
           , "  the RTP has the form: \"<mfa> when <guards> -> <actions>\""
           , "  where <mfa> can be;"
           , "  \"mod\", \"mod:fun\", \"mod:fun/3\" or \"mod:fun('_',atom,X)\""
           , "  <guard> is something like;"
           , "  \"X==1\" or \"is_atom(A)\""
           , "  and <action> is;"
           , "  \"return\" and/or \"stack\" (separated by \";\")"
           , ""
           , "  E.g."
           , "  ets:lookup(T,hostname) when is_integer(T) ->stack"
           , ""
           , "Opts: list({Opt,Val})"
           , "  general opts:"
           , "time         (15000)       stop trace after this many ms"
           , "msgs         (10)          stop trace after this many msgs"
           , "proc         (all)         (list of) Erlang process(es)"
           , "                           all|pid()|atom(RegName)|{pid,I2,I3}"
           , "target       (node())      node to trace on"
           , "arity        (false)       print arity instead of arg list"
           , "  print-related opts"
           , "max_queue    (5000)        fail if internal queue gets this long"
           , "max_msg_size (50000)       fail if seeing a msg this big"
           , "buffered     (no)          buffer messages till end of trace"
           , "print_calls  (true)        print calls"
           , "print_file   (standard_io) print to this file"
           , "print_msec   (false)       print milliseconds on timestamps"
           , "print_depth  (999999)      formatting depth for \"~P\""
           , "print_re     (\"\")          print only strings that match this"
           , "  trc file related opts"
           , "file         (none)        use a trc file based on this name"
           , "file_size    (1)           size of each trc file"
           , "file_count   (8)           number of trc files"
           , ""
          ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start from unix shell (e.g. the bin/redbug script)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unix([Node,Time,Msgs,Trc])      -> unix([Node,Time,Msgs,Trc,"all"]);
unix([Node,Time,Msgs,Trc,Proc]) ->
  try
    Cnf = #cnf{time = to_int(Time),
               msgs   = to_int(Msgs),
               trc    = try to_term(Trc) catch _:_ -> Trc end,
               proc   = to_atom(Proc),
               target = to_atom(Node)},
    self() ! {start,Cnf},
    init(),
    maybe_halt(0)
  catch
    exit:exiting ->
      maybe_halt(0);
    C:R ->
      io:fwrite("~p~n",[{C,R,erlang:get_stacktrace()}]),
      maybe_halt(1)
  end;
unix(X) ->
  io:fwrite("bad args: ~p~n",[X]),
  maybe_halt(1).

maybe_halt(Status) ->
  case is_in_shell() of
    true -> ok;
    false-> erlang:halt(Status)
  end.

is_in_shell() ->
  {_,{x,S}} = (catch erlang:error(x)),
  element(1,hd(lists:reverse(S))) == shell.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API from erlang shell
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
  case whereis(redbug) of
    undefined -> not_started;
    Pid -> Pid ! {stop,[]}
  end.

%% a bunch of aliases for start/2
start(Trc) -> start(Trc, []).

start(T,M,Trc) -> start(Trc, [{time,T},{msgs,M}]).

start(T,M,Trc,P) -> start(Trc, [{time,T},{msgs,M},{procs,P}]).

start(T,M,Trc,P,N)  -> start(Trc, [{time,T},{msgs,M},{procs,P},{target,N}]).

start(M,F) when is_atom(M), is_atom(F) -> start({M,F});
start(send,Props)                      -> start([send],Props);
start('receive',Props)                 -> start(['receive'],Props);
start(M,Props) when is_atom(M)         -> start([{M,'_'}],Props);
start(Trc,{Tag,Val})                   -> start(Trc, [{Tag,Val}]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the real start function!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Trc,Props) when is_list(Props) ->
  case whereis(redbug) of
    undefined ->
      Cnf = make_cnf(Trc,Props),
      assert_cookie(Cnf),
      try
        register(redbug, spawn(fun init/0)),
        redbug ! {start,Cnf},
        ok
      catch
        C:R -> {oops,{C,R}}
      end;
    _ -> redbug_already_started
  end.

assert_cookie(#cnf{cookie=''}) -> ok;
assert_cookie(Cnf) -> erlang:set_cookie(Cnf#cnf.target,Cnf#cnf.cookie).

%% turn the proplist inta a #cnf{}
make_cnf(Trc,Props) ->
  make_cnf(proplists:unfold(Props),#cnf{trc=Trc},record_info(fields,cnf)).

make_cnf([],Cnf,_) -> Cnf;
make_cnf([{Tag,Val}|Props],Cnf,Tags) ->
  make_cnf(Props,setelement(findex(Tag,Tags)+1,Cnf,Val),Tags).

findex(Tag,[])       -> exit({field_not_allowed,Tag});
findex(Tag,[Tag|_])  -> 1;
findex(Tag,[_|Tags]) -> findex(Tag,Tags)+1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the main redbug process
%%% a state machine. init, starting, running, stopping, maybe_stopping.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  process_flag(trap_exit,true),
  receive
    {start,Cnf} ->
      try
        starting(do_start(Cnf))
      catch
        C:R ->
          case {Cnf#cnf.debug,R} of
            {false,{X,Y,_}} -> erlang:display({X,Y});
            _               -> ?log([{C,R},{stack,erlang:get_stacktrace()}])

          end
      end
  end,
  exit(exiting).

starting(Cnf = #cnf{print_pid=PrintPid}) ->
  receive
    {stop,Args} -> prf:config(prf_redbug,prfTrc,{stop,{self(),Args}});
    {prfTrc,{starting,T,C}}      -> running(Cnf#cnf{trc_pid=T,cons_pid=C});
    {prfTrc,{already_started,_}} -> ?log(already_started);
    {'EXIT',PrintPid,R}          -> ?log([printer_died,{reason,R}]);
    {'EXIT',R}                   -> ?log([exited,{reason,R}]);
    X                            -> ?log([{unknown_message,X}])
  end.

running(Cnf = #cnf{trc_pid=TrcPid,print_pid=PrintPid}) ->
  Cnf#cnf.print_pid ! {trace_consumer,Cnf#cnf.cons_pid},
  receive
    {stop,Args} -> prf:config(prf_redbug,prfTrc,{stop,{self(),Args}}),
                   stopping(Cnf);
    {prfTrc,{stopping,_,_}}         -> stopping(Cnf);
    {'EXIT',TrcPid,_}               -> stopping(Cnf);
    {prfTrc,{not_started,R,TrcPid}} -> ?log([{not_started,R}]);
    {'EXIT',PrintPid,_}             -> maybe_stopping(Cnf);
    X                               -> ?log([{unknown_message,X}])
  end.

maybe_stopping(#cnf{trc_pid=TrcPid}) ->
  receive
    {prfTrc,{stopping,_,_}} -> ok;
    {'EXIT',TrcPid,_}       -> ok;
    X                       -> ?log({unknown_message,X})
  end.

stopping(#cnf{print_pid=PrintPid}) ->
  receive
    {'EXIT',PrintPid,_} -> ok;
    X                   -> ?log([{unknown_message,X}])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_start(OCnf) ->
  Cnf = spawn_printer(assert_print_fun(maybe_new_target(OCnf))),
  prf:start(prf_redbug,Cnf#cnf.target,redbugConsumer),
  prf:config(prf_redbug,prfTrc,{start,{self(),pack(Cnf)}}),
  Cnf.

maybe_new_target(Cnf = #cnf{target=Target}) ->
  case lists:member($@,Str=atom_to_list(Target)) of
    true -> Cnf;
    false-> Cnf#cnf{target=to_atom(Str++"@"++element(2,inet:gethostname()))}
  end.

assert_print_fun(Cnf) -> Cnf#cnf{print_fun=mk_print_fun(Cnf)}.

spawn_printer(Cnf) ->
  F = fun() -> print_init(fun(Ms) -> foreach(Cnf#cnf.print_fun,Ms) end) end,
  Cnf#cnf{print_pid=spawn_link(F)}.

mk_print_fun(#cnf{print_fun=PF}) when is_function(PF) -> PF;
mk_print_fun(Cnf)                                     -> mk_outer(Cnf).

mk_outer(#cnf{file=[_|_]}) ->
  fun(_) -> ok end;
mk_outer(#cnf{print_depth=Depth,print_msec=MS} = Cnf) ->
  OutFun = mk_out(Cnf),
  fun({Tag,Data,PI,TS}) ->
      MTS = fix_ts(MS,TS),
      case {Tag,Data} of
        {'call',{MFA,Bin}} ->
          case Cnf#cnf.print_calls of
            true ->
              OutFun("~n~s <~p> ~P",[MTS,PI,MFA,Depth]),
              foreach(fun(L)->OutFun("  ~s",[L]) end, stak(Bin));
            false->
              ok
          end;
        {'retn',{MFA,Val}} ->
          OutFun("~n~s <~p> ~p -> ~P",[MTS,PI,MFA,Val,Depth]);
        {'send',{MSG,To}} ->
          OutFun("~n~s <~p> <~p> <<< ~P",[MTS,PI,To,MSG,Depth]);
        {'recv',MSG} ->
          OutFun("~n~s <~p> <<< ~P",[MTS,PI,MSG,Depth])
      end
  end.

mk_out(#cnf{print_re=RE,print_file=File}) ->
  fun(F,A) ->
      Str=flat(F,A),
      case RE =:= "" andalso re:run(Str,RE) =:= nomatch of
        true  -> ok;
        false -> io:fwrite(get_fd(File),"~s~n",[Str])
      end
  end.

get_fd("") -> standard_io;
get_fd(FN) ->
  case file:open(FN,[write]) of
    {ok,FD} -> FD;
    _ -> exit({cannot_open,FN})
  end.

fix_ts(MS,TS) ->
  case MS of
    true -> ts_ms(TS);
    false-> ts(TS)
  end.

ts({H,M,S,_Us}) -> flat("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,S]).
ts_ms({H,M,S,Us}) -> flat("~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0w",[H,M,S,Us div 1000]).

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
  C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pack the cnf record into a proplist for prf consumption
%%% Proplist = list({Tag,Val})
%%% Tag = time | flags | rtps | procs | where
%%% Where = {term_buffer,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {term_stream,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {file,File,Size,Count} |
%%%         {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(Cnf) ->
  {Flags,RTPs} = foldl(fun chk_trc/2,{[],[]},slist(Cnf#cnf.trc)),
  dict:from_list([{time,chk_time(Cnf#cnf.time)},
                  {flags,[call,timestamp|maybe_arity(Cnf,Flags)]},
                  {rtps,RTPs},
                  {procs,chk_proc(Cnf#cnf.proc)},
                  {where,where(Cnf)}]).

where(Cnf) ->
  case Cnf#cnf.file of
    "" -> conf_term(Cnf);
    _  -> conf_file(Cnf)
  end.

conf_file(Cnf) ->
  {file,Cnf#cnf.file,Cnf#cnf.file_size,Cnf#cnf.file_count}.

conf_term(Cnf) ->
  {chk_buffered(Cnf#cnf.buffered),
   {Cnf#cnf.print_pid,
    chk_msgs(Cnf#cnf.msgs),
    Cnf#cnf.max_queue,
    Cnf#cnf.max_msg_size}}.

maybe_arity(#cnf{arity=true},Flags) -> [arity|Flags];
maybe_arity(_,Flags)                -> Flags.

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_buffered(true)  -> term_buffer;
chk_buffered(false) -> term_stream.

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom)-> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1), is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> exit({bad_proc,X}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> exit({bad_msgs,X}).

-define(is_string(Str), (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

chk_trc('send',{Flags,RTPs})                   -> {['send'|Flags],RTPs};
chk_trc('receive',{Flags,RTPs})                -> {['receive'|Flags],RTPs};
chk_trc('arity',{Flags,RTPs})                  -> {['arity'|Flags],RTPs};
chk_trc(RTP,{Flags,RTPs}) when ?is_string(RTP) -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(RTP,{Flags,RTPs}) when is_tuple(RTP)   -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(X,_)                                   -> exit({bad_trc,X}).

-define(is_aal(M,F,MS), is_atom(M),is_atom(F),is_list(MS)).

chk_rtp(Str) when ?is_string(Str)      -> redbug_msc:transform(Str);
chk_rtp({M})                           -> chk_rtp({M,'_',[]});
chk_rtp({M,F}) when is_atom(F)         -> chk_rtp({M,F,[]});
chk_rtp({M,L}) when is_list(L)         -> chk_rtp({M,'_',L});
chk_rtp({'_',_,_})                     -> exit(dont_wildcard_module);
chk_rtp({M,F,MS}) when ?is_aal(M,F,MS) -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X)                             -> exit({bad_rtp,X}).

ms(MS) -> foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack,[{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return,[{Head,Cond,Body}])-> [{Head,Cond,[{return_trace}|Body]}];
msf(Ari, [{_,Cond,Body}]) when is_integer(Ari)-> [{mk_head(Ari),Cond,Body}];
msf({Head,Cond},[{_,_,Body}]) when is_tuple(Head)->[{Head,slist(Cond),Body}];
msf(Head, [{_,Cond,Body}]) when is_tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> exit({bad_match_spec,X}).

mk_head(N) -> erlang:make_tuple(N,'_').

slist(S) when ?is_string(S) -> [S];
slist(L) when is_list(L) -> usort(L);
slist(X) -> [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_init(PrintFun) ->
  receive
    {trace_consumer,TC} ->
      erlang:monitor(process,TC),
      print_loop(PrintFun)
  end.

print_loop(PrintFun) ->
  receive
    {'DOWN',_,_,_,R} -> io:fwrite("quitting: ~p~n",[R]);
    X -> PrintFun(X),
         print_loop(PrintFun)
  end.

to_int(L) -> list_to_integer(L).
to_atom(L) -> list_to_atom(L).

to_term("_") -> '_';
to_term(Str) ->
  {done, {ok, Toks, 1}, []} = erl_scan:tokens([], "["++Str++"]. ", 1),
  case erl_parse:parse_term(Toks) of
    {ok, [Term]} -> Term;
    {ok, L} when is_list(L) -> L
  end.
