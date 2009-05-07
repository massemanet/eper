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

-import(lists,[reverse/1,foreach/2,map/2]).
-import(dict,[fetch/2
              , store/3
              , from_list/1]).

%% states
-define(ACTIVE, ?MODULE:active).
-define(IDLE, ?MODULE:idle).
-define(WAIT_FOR_LOCAL, ?MODULE:wait_for_local).

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
    undefined -> register(Reg,Pid=spawn_link(fun init/0)),Pid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace control process
%%% LD = idle | {host_pid,timer,consumer,conf}
%%% Conf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,{Pid,Count,MaxQueue,MaxSize}} | 
%%%         {term_stream,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {file,File,Size} | 
%%%         {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  process_flag(trap_exit,true),
  ?IDLE().

idle() ->
  receive
    {start,{HostPid,Conf}} -> ?ACTIVE(start_trace(HostPid,Conf));
    {stop,{HostPid,_}}     -> HostPid ! {prfTrc,{not_started,idle,self()}}, 
                              ?IDLE();
    X                      -> ?log({weird_in,X}), ?IDLE()
  end.

active({not_started,R,HostPid}) ->
  HostPid ! {prfTrc,{not_started,R,self()}},
  ?IDLE();
active(LD) ->
  Consumer = fetch(consumer,LD),
  HostPid = fetch(host_pid,LD),
  receive
    {start,{Pid,_}}     -> Pid ! {prfTrc,{already_started,self()}}, ?ACTIVE(LD);
    {stop,_}            -> remote_stop(Consumer, LD),?WAIT_FOR_LOCAL(Consumer);
    {'EXIT',HostPid,_}  -> remote_stop(Consumer, LD),?WAIT_FOR_LOCAL(Consumer);
    {local_stop,R}      -> local_stop(HostPid, LD, R),?WAIT_FOR_LOCAL(Consumer);
    {'EXIT',Consumer,R} -> local_stop(HostPid, LD, R),?IDLE();
    X                   -> ?log({weird_in,X}), ?ACTIVE(LD) 
  end.

wait_for_local(Consumer) when is_port(Consumer) ->
  ok;
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
  case maybe_load_modules(Conf) of
    []    -> {not_started,no_modules,HostPid};
    NConf -> start_trace(from_list([{host_pid,HostPid},{conf,NConf}]))
  end.
      
start_trace(LD) -> 
  Conf = fetch(conf,LD),
  HostPid = fetch(host_pid,LD),
  link(HostPid),
  Consumer = consumer(fetch(where,Conf),fetch(time,Conf)),
  HostPid ! {prfTrc,{starting,self(),Consumer}},
  Procs = mk_prc(fetch(procs,Conf)),
  Flags = [{tracer,Consumer}|fetch(flags,Conf)],
  unset_tps(),
  erlang:trace(Procs,true,Flags),
  untrace(family(redbug)++family(prfTrc),Flags),
  set_tps(fetch(rtps,Conf)),
  store(consumer,Consumer,LD).

maybe_load_modules(Conf) ->
  case lists:foldl(fun maybe_load_rtp/2, [], fetch(rtps,Conf)) of
    []   -> [];
    Rtps -> store(rtps,Rtps,Conf)
  end.

maybe_load_rtp({{M,F,A},_MatchSpec,_Flags} = Rtp,O) ->
  try 
    "/" = [hd(code:which(M))],
    [c:l(M) || false == code:is_loaded(M)],
    [Rtp|O]
  catch 
    error:R -> ?log({no_such_function,{R,{M,F,A}}}), O
  end.

family(Daddy) ->
  try D = whereis(Daddy), 
      [D|element(2,process_info(D,links))] 
  catch _:_->[] 
  end.

untrace(Pids,Flags) ->
  [try erlang:trace(P,false,Flags)
   catch _:R->erlang:display({R,process_info(P),erlang:trace_info(P,flags)})
   end || P <- Pids,
	   node(P)==node(),
	   {flags,[]}=/=erlang:trace_info(P,flags)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consumer({term_buffer,Term},Time) -> consumer_pid(Term,yes,Time);
consumer({term_stream,Term},Time) -> consumer_pid(Term,no,Time);
consumer({file,File,Size},_) -> consumer_file(File,Size);
consumer({ip,Port,Queue},_) -> consumer_ip(Port,Queue).

consumer_stop(Pid) when is_pid(Pid) -> Pid ! stop;
consumer_stop(_Port) when is_port(_Port) -> dbg:flush_trace_port().

consumer_pid({Pid,Cnt,MaxQueue,MaxSize},Buf,Time) ->
  Conf = from_list([{daddy,self()},
                    {count,Cnt},
                    {time,Time},
                    {maxsize,MaxSize},
                    {maxqueue,MaxQueue},
                    {where,Pid},
                    {buffering,Buf}]),
  spawn_link(fun() -> init_local(Conf) end).

consumer_file(File,Size) ->
  %% number of files
  WrapCnt = 2,
  %% file size (per file). Size is given in Mb.
  WrapSize = Size*1024*1024,
  Suffix = ".trc",
  (dbg:trace_port(file,{File, wrap, Suffix, WrapSize, WrapCnt}))().

consumer_ip(Port,QueueSize) ->
  %% keep at most this many in the buffer on the sender side
  (dbg:trace_port(ip,{Port, QueueSize}))().

unset_tps() ->
  erlang:trace_pattern({'_','_','_'},false,[local]),
  erlang:trace_pattern({'_','_','_'},false,[global]).

set_tps(TPs) -> foreach(fun set_tps_f/1,TPs).

set_tps_f({MFA,MatchSpec,Flags}) -> erlang:trace_pattern(MFA,MatchSpec,Flags).

mk_prc(all) -> all;
mk_prc(Pid) when is_pid(Pid) -> Pid;
mk_prc({pid,P1,P2}) when is_integer(P1), is_integer(P2) -> c:pid(0,P1,P2);
mk_prc(Reg) when is_atom(Reg) -> 
  case whereis(Reg) of 
    undefined -> exit({no_such_process, Reg});
    Pid when is_pid(Pid) -> Pid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  the local consumer process. 
%%%  buffers trace messages, and flushes them when;
%%%    it gets a stop from the controller
%%%    reaches count=0
%%%    timeout
%%%    message queue too long
%%%    a trace message is too big
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
-record(ld,{daddy,where,count,maxqueue,maxsize}).

init_local(Conf) -> 
  erlang:start_timer(fetch(time,Conf),self(),fetch(daddy,Conf)),
  lloop({#ld{daddy=fetch(daddy,Conf),
	     where=fetch(where,Conf),
	     maxsize=fetch(maxsize,Conf),
	     maxqueue=fetch(maxqueue,Conf)},
	 buffering(fetch(buffering,Conf)),
	 fetch(count,Conf)}).

buffering(yes) -> [];
buffering(no) -> no.

lloop({LD,Buff,Count}) ->
  maybe_exit(msg_queue,LD),
  maybe_exit(msg_count,{LD,Buff,Count}),
  receive 
    {timeout,_,Daddy}        -> Daddy ! {local_stop,timeout},
				flush(LD,Buff),exit(timeout);
    stop 		     -> flush(LD,Buff),exit(local_done);
    {trace_ts,Pid,Tag,A,TS}  -> lloop(msg(LD,Buff,Count,{Tag,Pid,TS,A}));
    {trace_ts,Pid,Tag,A,B,TS}-> lloop(msg(LD,Buff,Count,{Tag,Pid,TS,{A,B}}))
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
  case process_info(self(),message_queue_len) of
    {_,Q} when Q > MQ -> exit({msg_queue,Q});
    _ -> ok
  end;
maybe_exit(msg_size,{#ld{maxsize=MS},{call,_,_,{_,B}}}) when is_binary(B)-> 
  case MS < (BS=size(B)) of
    true -> exit({msg_size,BS});
    _ -> ok
  end;
maybe_exit(_,_) -> ok.

send_one(LD,Msg) -> LD#ld.where ! [msg(Msg)].

flush(_,no) -> ok;
flush(LD,Buffer) -> LD#ld.where ! map(fun msg/1, reverse(Buffer)).

msg({'send',Pid,TS,{Msg,To}}) ->       {'send',{Msg,pi(To)},pi(Pid),ts(TS)};
msg({'receive',Pid,TS,Msg}) ->         {'recv',Msg,         pi(Pid),ts(TS)};
msg({'return_from',Pid,TS,{MFA,V}}) -> {'retn',{MFA,V},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,{MFA,B}}) ->        {'call',{MFA,B},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,MFA}) ->            {'call',{MFA,<<>>},  pi(Pid),ts(TS)}.

pi(P) when is_pid(P) ->
  try process_info(P, registered_name) of
      [] -> case process_info(P, initial_call) of
              {_, {proc_lib,init_p,5}} -> proc_lib:translate_initial_call(P);
              {_,MFA} -> MFA;
              undefined -> dead
            end;
      {_,Nam} -> Nam;
      undefined -> dead
  catch 
    error:badarg -> node(P)
  end;
pi(P) when is_port(P) -> 
  {name,N} = erlang:port_info(P,name),
  [Hd|_] = string:tokens(N," "),
  reverse(hd(string:tokens(reverse(Hd),"/")));
pi(R) when is_atom(R) -> R;
pi({R,Node}) when is_atom(R), Node == node() -> R;
pi({R, Node}) when is_atom(R), is_atom(Node) -> {R, Node}.

ts(Nw) ->
  {_,{H,M,S}} = calendar:now_to_local_time(Nw),
  {H,M,S,element(3,Nw)}.
