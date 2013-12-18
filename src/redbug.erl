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

-include("log.hrl").

%% the redbug server data structure
%% most can be set in the input proplist
-record(cnf,{
          %% general
          time         = 15000,          % stop trace after this time [ms]
          msgs         = 10,           % stop trace after this # msgs [unit]
          target       = node(),       % target node
          cookie       = '',           % target node cookie
          blocking     = false,        % run blocking; return a list of msgs
          procs        = all,          % list of procs (or 'all')
          max_queue    = 5000,         % max # of msgs before suicide
          max_msg_size = 50000,        % max message size before suicide
          debug        = false,        % big error messages
          %% print-related
          arity        = false,        % arity instead of args
          buffered     = false,        % output buffering
          print_calls  = true,         % print calls
          print_file   = "",           % file to print to (standard_io)
          print_msec   = false,        % print milliseconds in timestamps?
          print_depth  = 999999,       % Limit for "~P" formatting depth
          print_re     = "",           % regexp that must match to print
          print_fun    = '',           % custom print handler
          %% trc file-related
          file         = "",           % file to write trace msgs to
          file_size    = 1,            % file size (per file [Mb])
          file_count   = 8,            % number of files in wrap log
          %% internal
          trc          = [],           % cannot be set by user
          shell_pid    = [],           % cannot be set by user
          print_pid    = [],           % cannot be set by user
          trc_pid      = [],           % cannot be set by user
          cons_pid     = []            % cannot be set by user
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help() ->
  Text =
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
     , "target       (node())      node to trace on"
     , "blocking     (false)       block start/2, return a list of messages"
     , "max_queue    (5000)        fail if internal queue gets this long"
     , "max_msg_size (50000)       fail if seeing a msg this big"
     , "procs        (all)         (list of) Erlang process(es)"
     , "                             all|pid()|atom(RegName)|{pid,I2,I3}"
     , "  print-related opts"
     , "arity        (false)       print arity instead of arg list"
     , "buffered     (false)       buffer messages till end of trace"
     , "print_calls  (true)        print calls"
     , "print_file   (standard_io) print to this file"
     , "print_msec   (false)       print milliseconds on timestamps"
     , "print_depth  (999999)      formatting depth for \"~P\""
     , "print_re     (\"\")        print only strings that match this RE"
     , "print_fun    ()            custom print handler, fun/1 or fun/2;"
     , "                             fun(TrcMsg) -> <ignored>"
     , "                             fun(TrcMsg,AccOld) -> AccNew"
     , "  trc file related opts"
     , "file         (none)        use a trc file based on this name"
     , "file_size    (1)           size of each trc file"
     , "file_count   (8)           number of trc files"
     , ""
    ],
  lists:foreach(fun(S)->io:fwrite(standard_io,"~s~n",[S])end,Text).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start from unix shell (e.g. the bin/redbug script)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unix([Node,Trc])                -> unix([Node,Trc,"15000"]);
unix([Node,Trc,Time])           -> unix([Node,Trc,Time,"10"]);
unix([Node,Trc,Time,Msgs])      -> unix([Node,Trc,Time,Msgs,"all"]);
unix([Node,Trc,Time,Msgs,Proc]) ->
  try
    Cnf = #cnf{time = to_int(Time),
               msgs      = to_int(Msgs),
               trc       = try to_term(Trc) catch _:_ -> Trc end,
               procs     = [to_atom(Proc)],
               target    = to_atom(Node),
               print_fun = mk_outer(#cnf{})},
    self() ! {start,Cnf},
    init(),
    maybe_halt(0)
  catch
    C:R ->
      io:fwrite("~p~n",[{C,R,erlang:get_stacktrace()}]),
      maybe_halt(1)
  end;
unix(X) ->
  io:fwrite("bad args: ~p~n",[X]),
  maybe_halt(1).

to_term("_") -> '_';
to_term(Str) ->
  {done, {ok, Toks, 1}, []} = erl_scan:tokens([], "["++Str++"]. ", 1),
  case erl_parse:parse_term(Toks) of
    {ok, [Term]} -> Term;
    {ok, L} when is_list(L) -> L
  end.

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
    Pid -> Pid ! {stop,[]},stopped
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
      try
        Cnf = assert_print_fun(make_cnf(Trc,[{shell_pid,self()}|Props])),
        assert_cookie(Cnf),
        register(redbug, spawn(fun init/0)),
        redbug ! {start,Cnf},
        maybe_block(Cnf,block_a_little())
      catch
        R   -> R;
        C:R -> {oops,{C,R}}
      end;
    _ ->
      redbug_already_started
  end.

assert_print_fun(Cnf) ->
  case is_function(Cnf#cnf.print_fun) of
    true -> Cnf;
    false-> Cnf#cnf{print_fun=make_print_fun(Cnf)}
  end.

make_print_fun(Cnf) ->
  case Cnf#cnf.blocking of
    false-> mk_outer(Cnf);
    true -> fun(X,0) -> [X]; (X,A) -> [X|A] end
  end.

assert_cookie(#cnf{cookie=''}) -> ok;
assert_cookie(Cnf) -> erlang:set_cookie(Cnf#cnf.target,Cnf#cnf.cookie).

block_a_little() ->
  Ref = erlang:monitor(process,redbug),
  receive
    {running,NoP,NoF}  -> erlang:demonitor(Ref), {NoP,NoF};
    {'DOWN',Ref,_,_,R} -> R
  end.

maybe_block(#cnf{blocking=true},{I,_}) when is_integer(I) -> block();
maybe_block(_,R) -> R.

block() ->
  Ref = erlang:monitor(process,redbug),
  receive
    {'DOWN',Ref,_,_,R} -> R
  end.

%% turn the proplist inta a #cnf{}
make_cnf(Trc,Props) ->
  make_cnf(proplists:unfold(Props),#cnf{trc=Trc},record_info(fields,cnf)).

make_cnf([],Cnf,_) -> Cnf;
make_cnf([{Tag,Val}|Props],Cnf,Tags) ->
  make_cnf(Props,setelement(findex(Tag,Tags)+1,Cnf,Val),Tags).

findex(Tag,[])       -> throw({no_such_option,Tag});
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
        R ->
          exit({argument_error,R});
        C:R ->
          case Cnf#cnf.debug of
            false-> ok;
            true -> ?log([{C,R},{stack,erlang:get_stacktrace()}])
          end,
          exit(R)
      end
  end.

starting(Cnf = #cnf{print_pid=PrintPid}) ->
  receive
    {stop,Args} -> prf:config(prf_redbug,prfTrc,{stop,{self(),Args}});
    {prfTrc,{starting,P,F,T,C}}  -> running(Cnf#cnf{trc_pid=T,cons_pid=C},P,F);
    {'EXIT',_,{prfTrc,R}}        -> throw(R);
    {prfTrc,{already_started,_}} -> ?log(already_started);
    {'EXIT',PrintPid,R}          -> ?log([printer_died,{reason,R}]);
    {'EXIT',R}                   -> ?log([exited,{reason,R}]);
    X                            -> ?log([{unknown_message,X}])
  end.

running(Cnf = #cnf{trc_pid=TrcPid,print_pid=PrintPid},P,F) ->
  Cnf#cnf.print_pid ! {trace_consumer,Cnf#cnf.cons_pid},
  [Cnf#cnf.shell_pid ! {running,P,F} || is_pid(Cnf#cnf.shell_pid)],
  receive
    {stop,Args} -> prf:config(prf_redbug,prfTrc,{stop,{self(),Args}}),
                   stopping(Cnf);
    {prfTrc,{stopping,_,_}}         -> stopping(Cnf);
    {'EXIT',TrcPid,R}               -> ?log({trace_control_died,R}),
                                       stopping(Cnf);
    {prfTrc,{not_started,R,TrcPid}} -> ?log([{not_started,R}]);
    {'EXIT',PrintPid,R}             -> wait_for_trc(Cnf,R);
    X                               -> ?log([{unknown_message,X}])
  end.

wait_for_trc(Cnf = #cnf{trc_pid=TrcPid},R) ->
  receive
    {prfTrc,{stopping,_,_}} -> done(Cnf,R);
    {'EXIT',TrcPid,R}       -> ?log({trace_control_died,R});
    X                       -> ?log({unknown_message,X})
  end.

stopping(Cnf = #cnf{print_pid=PrintPid}) ->
  receive
    {'EXIT',PrintPid,R} -> done(Cnf,R);
    X                   -> ?log([{unknown_message,X}])
  end.

done(#cnf{blocking=false},{R,A}) ->
  io:fwrite("redbug done, ~p - ~p~n",[R,A]);
done(#cnf{blocking=true},R) ->
  exit(R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_start(OCnf) ->
  Cnf = spawn_printer(wrap_print_fun(OCnf),maybe_new_target(OCnf)),
  prf:start(prf_redbug,Cnf#cnf.target,redbugConsumer),
  prf:config(prf_redbug,prfTrc,{start,{self(),pack(Cnf)}}),
  Cnf.

maybe_new_target(Cnf = #cnf{target=Target}) ->
  case lists:member($@,Str=atom_to_list(Target)) of
    true -> Cnf;
    false-> Cnf#cnf{target=to_atom(Str++"@"++element(2,inet:gethostname()))}
  end.

spawn_printer(PrintFun,Cnf) ->
  Cnf#cnf{print_pid=spawn_link(fun() -> print_init(PrintFun) end)}.

wrap_print_fun(#cnf{print_fun=PF}) ->
  case erlang:fun_info(PF,arity) of
    {arity,1} -> fun(M,N) -> PF(M),N+1 end;
    {arity,2} -> PF
  end.

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
              OutFun("~n~s ~s ~P",[MTS,to_str(PI),MFA,Depth]),
              lists:foreach(fun(L)->OutFun("  ~s",[L]) end, stak(Bin));
            false->
              ok
          end;
        {'retn',{{M,F,A},Val}} ->
          OutFun("~n~s ~s ~p:~p/~p -> ~P",[MTS,to_str(PI),M,F,A,Val,Depth]);
        {'send',{MSG,ToPI}} ->
          OutFun("~n~s ~s ~s <<< ~P",
                 [MTS,to_str(PI),to_str(ToPI),MSG,Depth]);
        {'recv',MSG} ->
          OutFun("~n~s ~s <<< ~P",[MTS,to_str(PI),MSG,Depth])
      end
  end.

to_str({Pid,Reg}) -> flat("~w(~p)",[Pid,Reg]).

mk_out(#cnf{print_re=RE,print_file=File}) ->
  FD = get_fd(File),
  fun(F,A) ->
      Str=flat(F,A),
      case RE =:= "" orelse re:run(Str,RE) =/= nomatch of
        true  -> io:fwrite(FD,"~s~n",[Str]);
        false -> ok
      end
  end.

get_fd("") -> standard_io;
get_fd(FN) ->
  case file:open(FN,[append]) of
    {ok,FD} -> FD;
    _ -> throw({cannot_open,FN})
  end.

fix_ts(MS,TS) ->
  case MS of
    true -> ts_ms(TS);
    false-> ts(TS)
  end.

ts({H,M,S,_Us}) -> flat("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,S]).
ts_ms({H,M,S,Us}) -> flat("~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0w",[H,M,S,Us div 1000]).

flat(Form,List) ->
  lists:flatten(io_lib:fwrite(Form,List)).


%%% call stack handler
stak(Bin) ->
  lists:foldl(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case lists:reverse(I) of
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
  {Flags,RTPs} = lists:foldl(fun chk_trc/2,{[],[]},slist(Cnf#cnf.trc)),
  dict:from_list([{time,chk_time(Cnf#cnf.time)},
                  {flags,[call,timestamp|maybe_arity(Cnf,Flags)]},
                  {rtps,RTPs},
                  {procs,[chk_proc(P) || P <- mk_list(Cnf#cnf.procs)]},
                  {where,where(Cnf)}]).

mk_list([]) -> throw(no_procs);
mk_list([_|_] = L) -> L;
mk_list(E) -> [E].

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
chk_time(X) -> throw({bad_time,X}).

chk_buffered(true)  -> term_buffer;
chk_buffered(false) -> term_stream.

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom)-> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1), is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> throw({bad_proc,X}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> throw({bad_msgs,X}).

-define(is_string(Str), (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

chk_trc('send',{Flags,RTPs})                   -> {['send'|Flags],RTPs};
chk_trc('receive',{Flags,RTPs})                -> {['receive'|Flags],RTPs};
chk_trc('arity',{Flags,RTPs})                  -> {['arity'|Flags],RTPs};
chk_trc(RTP,{Flags,RTPs}) when ?is_string(RTP) -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(RTP,{Flags,RTPs}) when is_tuple(RTP)   -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(X,_)                                   -> throw({bad_trc,X}).

-define(is_aal(M,F,MS), is_atom(M),is_atom(F),is_list(MS)).

chk_rtp(Str) when ?is_string(Str)      -> redbug_msc:transform(Str);
chk_rtp({M})                           -> chk_rtp({M,'_',[]});
chk_rtp({M,F}) when is_atom(F)         -> chk_rtp({M,F,[]});
chk_rtp({M,L}) when is_list(L)         -> chk_rtp({M,'_',L});
chk_rtp({'_',_,_})                     -> throw(dont_wildcard_module);
chk_rtp({M,F,MS}) when ?is_aal(M,F,MS) -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X)                             -> throw({bad_rtp,X}).

ms(MS) -> lists:foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack,[{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return,[{Head,Cond,Body}])-> [{Head,Cond,[{return_trace}|Body]}];
msf(Ari, [{_,Cond,Body}]) when is_integer(Ari)-> [{mk_head(Ari),Cond,Body}];
msf({Head,Cond},[{_,_,Body}]) when is_tuple(Head)->[{Head,slist(Cond),Body}];
msf(Head, [{_,Cond,Body}]) when is_tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> throw({bad_match_spec,X}).

mk_head(N) -> erlang:make_tuple(N,'_').

slist(S) when ?is_string(S) -> [S];
slist(L) when is_list(L) -> lists:usort(L);
slist(X) -> [X].

to_int(L) -> list_to_integer(L).
to_atom(L) -> list_to_atom(L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the print_loop process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_init(PrintFun) ->
  receive
    {trace_consumer,TC} ->
      erlang:monitor(process,TC),
      print_loop(PrintFun,0)
  end.

print_loop(PrintFun,Acc) ->
  receive
    {'DOWN',_,_,_,R} -> exit({R,Acc});
    X                -> print_loop(PrintFun,lists:foldl(PrintFun,Acc,X))
  end.
