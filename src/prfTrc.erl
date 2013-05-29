%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : prfTrc.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 18 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(prfTrc).

-export([collect/1,config/2]).
%% internal
-export([active/1,idle/0,wait_for_local/1]).

-import(dict,[fetch/2
              , store/3
              , from_list/1]).

%% states
-define(ACTIVE         , ?MODULE:active).
-define(IDLE           , ?MODULE:idle).
-define(WAIT_FOR_LOCAL , ?MODULE:wait_for_local).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runs in the prfTarg process

collect(LD) -> {LD, {?MODULE, {tick,now()}}}.

config(LD,{start,Conf}) -> start(Conf),LD;
config(LD,{stop,Args}) -> stop(Args),LD;
config(LD,Data) -> ?log([unknown,{data,Data}]), LD.

start(Conf) ->
  assert(prfTrc) ! {start, Conf}.

stop(Args) ->
  assert(prfTrc) ! {stop,Args}.

assert(Reg) ->
  case whereis(Reg) of
    Pid when is_pid(Pid) -> Pid;
    undefined            -> register(Reg,Pid=spawn_link(fun init/0)),
                            Pid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace control process
%%% LD = idle | {host_pid,timer,consumer,conf}
%%% Conf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {term_stream,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {file,File,Size,Count} |
%%%         {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  process_flag(trap_exit,true),
  ?IDLE().

idle() ->
  receive
    {start,{HostPid,Conf}} -> link(HostPid),
                              ?ACTIVE(start_trace(HostPid,Conf));
    {stop,{HostPid,_}}     -> HostPid ! {prfTrc,{not_started,idle,self()}},
                              ?IDLE();
    {'EXIT',_,normal}      -> ?IDLE();
    X                      -> ?log({weird_in,X}), ?IDLE()
  end.

active({not_started,R,HostPid}) ->
  HostPid ! {prfTrc,{not_started,R,self()}},
  ?IDLE();
active(LD) ->
  Cons = fetch(consumer,LD),
  HostPid = fetch(host_pid,LD),
  receive
    {start,{Pid,_}}     -> Pid ! {prfTrc,{already_started,self()}},?ACTIVE(LD);
    {stop,_}            -> remote_stop(Cons,LD),?WAIT_FOR_LOCAL(Cons);
    {'EXIT',HostPid,_}  -> remote_stop(Cons,LD),?WAIT_FOR_LOCAL(Cons);
    {local_stop,R}      -> local_stop(HostPid,LD,R),?WAIT_FOR_LOCAL(Cons);
    {'EXIT',Cons,R}     -> local_stop(HostPid,LD,R),?IDLE();
    X                   -> ?log({weird_in,X}), ?ACTIVE(LD)
  end.

wait_for_local(Consumer) when is_pid(Consumer) ->
  receive
    {'EXIT',Consumer,_} -> ?IDLE();
    X                   -> ?log({weird_in,X}), ?WAIT_FOR_LOCAL(Consumer)
  end.

local_stop(HostPid, LD, R) ->
  stop_trace(LD),
  unlink(HostPid),
  HostPid ! {prfTrc,{stopping,self(),R}}.

remote_stop(Consumer, LD) ->
  stop_trace(LD),
  consumer_stop(Consumer).

stop_trace(LD) ->
  erlang:trace(all,false,fetch(flags,fetch(conf,LD))),
  unset_tps().

start_trace(HostPid,Conf) ->
  Rtps = maybe_load_rtps(fetch(rtps,Conf)),
  start_trace(from_list([{host_pid,HostPid},{conf,store(rtps,Rtps,Conf)}])).

maybe_load_rtps(Rtps) ->
  lists:foldl(fun maybe_load_rtp/2, [], Rtps).

maybe_load_rtp({{M,_,_},_MatchSpec,_Flags} = Rtp,O) ->
  try
    case code:which(M) of
      preloaded         -> ok;
      non_existing      -> throw(non_existing_module);
      L when is_list(L) -> [c:l(M) || false == code:is_loaded(M)]
    end,
    [Rtp|O]
  catch
    _:_ -> O
  end.

start_trace(LD) ->
  Conf = fetch(conf,LD),
  Consumer = consumer(fetch(where,Conf),fetch(time,Conf)),
  Ps = lists:foldl(fun mk_prc/2,[],fetch(procs,Conf)),
  Rtps = fetch(rtps,Conf),
  Flags = [{tracer,real_consumer(Consumer)}|fetch(flags,Conf)],
  unset_tps(),
  NoProcs = lists:sum([erlang:trace(P,true,Flags) || P <- Ps]),
  untrace(family(redbug)++family(prfTrc),Flags),
  NoFuncs = set_tps(Rtps),
  assert_trace_targets(NoProcs,NoFuncs,Flags),
  fetch(host_pid,LD) ! {prfTrc,{starting,NoProcs,NoFuncs,self(),Consumer}},
  store(consumer,Consumer,LD).

family(Daddy) ->
  try D = whereis(Daddy),
      [D|element(2,process_info(D,links))]
  catch _:_->[]
  end.

untrace(Pids,Flags) ->
  [try erlang:trace(P,false,Flags)
   catch _:R-> erlang:display({R,process_info(P),erlang:trace_info(P,flags)})
   end || P <- Pids,
          is_pid(P),
          node(P)==node(),
          {flags,[]}=/=erlang:trace_info(P,flags)].

assert_trace_targets(NoProcs,NoFuncs,Flags) ->
  case 0 < NoProcs of
    true -> ok;
    false-> exit({prfTrc,no_matching_processes})
  end,
  case 0 < NoFuncs orelse is_message_trace(Flags) of
    true -> ok;
    false-> exit({prfTrc,no_matching_functions})
  end.

is_message_trace(Flags) ->
  (lists:member(send,Flags) orelse lists:member('receive',Flags)).

unset_tps() ->
  erlang:trace_pattern({'_','_','_'},false,[local]),
  erlang:trace_pattern({'_','_','_'},false,[global]).

set_tps(TPs) ->
  lists:foldl(fun set_tps_f/2,0,TPs).

set_tps_f({MFA,MatchSpec,Flags},A) ->
  A+erlang:trace_pattern(MFA,MatchSpec,Flags).

mk_prc(all,A) ->
  [all|A];
mk_prc(Reg,A) when is_atom(Reg) ->
  case whereis(Reg) of
    Pid when is_pid(Pid) -> mk_prc(Pid,A);
    undefined -> A
  end;
mk_prc({pid,P1,P2},A) when is_integer(P1), is_integer(P2) ->
  mk_prc(c:pid(0,P1,P2),A);
mk_prc(Pid,A) when is_pid(Pid) ->
  case is_process_alive(Pid) of
    true -> [Pid|A];
    false-> A
  end.

real_consumer(C) ->
  Mon = erlang:monitor(process,C),
  C ! {show_port, self()},
  receive
    {'DOWN',Mon,_,C,R} -> exit({no_local_consumer,R});
    Port               -> erlang:demonitor(Mon,[flush]),
                          Port
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consumer({term_buffer,Term},Time)     -> consumer_pid(Term,yes,Time);
consumer({term_stream,Term},Time)     -> consumer_pid(Term,no,Time);
consumer({file,File,Size,Count},Time) -> consumer_file(File,Size,Count,Time);
consumer({ip,Port,Queue},Time)        -> consumer_ip(Port,Queue,Time).

consumer_stop(Pid) -> Pid ! stop.

consumer_pid({Pid,Cnt,MaxQueue,MaxSize},Buf,Time) ->
  Conf =
    from_list([{daddy,self()},
               {count,Cnt},
               {time,Time},
               {maxsize,MaxSize},
               {maxqueue,MaxQueue},
               {where,Pid},
               {buffering,Buf}]),
  spawn_link(fun() -> init_local_pid(Conf) end).

consumer_file(File,Size,WrapCount,Time) ->
  Conf =
    from_list([{style,file}
               , {file,File}
               , {size,Size}
               , {wrap_count,WrapCount}
               , {time,Time}
               , {daddy, self()}]),
  spawn_link(fun() -> init_local_port(Conf) end).

consumer_ip(Port,QueueSize,Time) ->
  Conf =
    from_list([{style,ip}
               , {port_no,Port}
               , {queue_size,QueueSize}
               , {time,Time}
               , {daddy, self()}]),
  spawn_link(fun() -> init_local_port(Conf) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  local consumer process for port-style tracing.
%%%  writes trace messages directly to an erlang port.
%%%  flushes and quits when;
%%%    it gets a stop from the controller
%%%    timeout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_local_port(Conf) ->
  erlang:start_timer(fetch(time,Conf),self(),fetch(daddy,Conf)),
  Port = mk_port(Conf),
  loop_local_port(store(port,Port,Conf)).

loop_local_port(Conf) ->
  Daddy = fetch(daddy, Conf),
  receive
    {show_port,Pid}   -> Pid ! fetch(port,Conf),
                         loop_local_port(Conf);
    stop              -> dbg:flush_trace_port(),
                         exit(local_done);
    {timeout,_,Daddy} -> Daddy ! {local_stop,timeout},
                         dbg:flush_trace_port(),
                         exit(timeout)
  end.

mk_port(Conf) ->
  case fetch(style,Conf) of
    ip ->
      Port = fetch(port_no,Conf),
      QueueSize = fetch(queue_size,Conf),
      (dbg:trace_port(ip,{Port, QueueSize}))();
    file ->
      File = fetch(file,Conf),
      WrapCount = fetch(wrap_count,Conf),
      WrapSize = fetch(size,Conf)*1024*1024,% file size (per file) in MB.
      Suffix = ".trc",
      (dbg:trace_port(file,{File, wrap, Suffix, WrapSize, WrapCount}))()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  local consumer process for pid-style tracing.
%%%  buffers trace messages, and flushes them when;
%%%    it gets a stop from the controller
%%%    reaches count=0
%%%    timeout
%%%    message queue too long
%%%    a trace message is too big
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
-record(ld,{daddy,where,count,maxqueue,maxsize}).

init_local_pid(Conf) ->
  erlang:start_timer(fetch(time,Conf),self(),fetch(daddy,Conf)),
  loop_lp({#ld{daddy=fetch(daddy,Conf),
               where=fetch(where,Conf),
               maxsize=fetch(maxsize,Conf),
               maxqueue=fetch(maxqueue,Conf)},
           buffering(fetch(buffering,Conf)),
           fetch(count,Conf)}).

buffering(yes) -> [];
buffering(no) -> no.

loop_lp({LD,Buff,Count}=State) ->
  maybe_exit(msg_queue,LD),
  maybe_exit(msg_count,{LD,Buff,Count}),
  receive
    {timeout,_,Daddy}         -> Daddy ! {local_stop,timeout},
                                 flush(LD,Buff),exit(timeout);
    stop                      -> flush(LD,Buff),exit(local_done);
    {show_port,Pid}           -> Pid ! self(),
                                 loop_lp(State);
    {trace_ts,Pid,Tag,A,TS}   -> loop_lp(msg(LD,Buff,Count,{Tag,Pid,TS,A}));
    {trace_ts,Pid,Tag,A,B,TS} -> loop_lp(msg(LD,Buff,Count,{Tag,Pid,TS,{A,B}}))
  end.

msg(LD,Buff,Count,Item) ->
  maybe_exit(msg_size,{LD,Item}),
  {LD,buff(Buff,LD,Item),Count-1}.

buff(no,LD,Item) -> send_one(LD,Item),no;
buff(Buff,_LD,Item) -> [Item|Buff].

maybe_exit(msg_count,{LD,Buff,0}) ->
  flush(LD,Buff),
  exit(msg_count);
maybe_exit(msg_queue,#ld{maxqueue=MQ}) ->
  maybe_exit_queue(MQ);
maybe_exit(msg_size,{#ld{maxsize=MS},{call,_,_,{MFA,B}}}) when is_binary(B)->
  maybe_exit_call(MS,MFA,B);
maybe_exit(msg_size,{#ld{maxsize=MS},{call,_,_,MFA}}) ->
  maybe_exit_call(MS,MFA,<<>>);
maybe_exit(_,_) -> ok.

%% check the message queue length
maybe_exit_queue(MQ) ->
  case process_info(self(),message_queue_len) of
    {_,Q} when Q > MQ -> exit({msg_queue,Q});
    _ -> ok
  end.

%% can we handle the call trace msg, or is it too big?
maybe_exit_call(MS,{_M,_F,A},B) ->
  maybe_exit_stack(MS,B),
  maybe_exit_args(MS,A).

%% check the stack binary
maybe_exit_stack(MS,B) ->
  case MS < (Sz=size(B)) of
    true -> exit({stack_size,Sz});
    false-> ok
  end.

%% recurse through the args
%% exit if there is a long list or a large binary
maybe_exit_args(MS,T) when is_tuple(T) ->
  maybe_exit_args(MS,tuple_to_list(T));
maybe_exit_args(MS,L) when length(L) < MS ->
  lists:foreach(fun(E)->maybe_exit_args(MS,E)end,L);
maybe_exit_args(MS,L) when MS =< length(L) ->
  exit({arg_length,length(L)});
maybe_exit_args(MS,B) when MS < byte_size(B) ->
  exit({arg_size,byte_size(B)});
maybe_exit_args(_,_) ->
  ok.

send_one(LD,Msg) -> LD#ld.where ! [msg(Msg)].

flush(_,no) -> ok;
flush(LD,Buffer) ->
  LD#ld.where ! lists:map(fun msg/1,lists:reverse(Buffer)).

msg({'send',Pid,TS,{Msg,To}})          -> {'send',{Msg,pi(To)},pi(Pid),ts(TS)};
msg({'receive',Pid,TS,Msg})            -> {'recv',Msg,         pi(Pid),ts(TS)};
msg({'return_from',Pid,TS,{MFA,V}})    -> {'retn',{MFA,V},     pi(Pid),ts(TS)};
msg({'exception_from',Pid,TS,{MFA,V}}) -> {'retn',{MFA,V},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,{MFA,B}})           -> {'call',{MFA,B},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,MFA})               -> {'call',{MFA,<<>>},  pi(Pid),ts(TS)}.

pi(P) when is_pid(P) ->
  try process_info(P, registered_name) of
      [] ->
        case process_info(P, initial_call) of
          {_, {proc_lib,init_p,5}} -> {P,proc_lib:translate_initial_call(P)};
          {_,MFA}                  -> {P,MFA};
          undefined                -> {P,dead}
        end;
      {_,Nam}   -> {P,Nam};
      undefined -> {P,dead}
  catch
    error:badarg -> {P,node(P)}
  end;
pi(P) when is_port(P) ->
  {name,N} = erlang:port_info(P,name),
  [Hd|_] = string:tokens(N," "),
  {P,lists:reverse(hd(string:tokens(lists:reverse(Hd),"/")))};
pi(R) when is_atom(R) -> R;
pi({R,Node}) when is_atom(R), Node == node() -> R;
pi({R,Node}) when is_atom(R), is_atom(Node) -> {R,Node}.

ts(Nw) ->
  {_,{H,M,S}} = calendar:now_to_local_time(Nw),
  {H,M,S,element(3,Nw)}.
