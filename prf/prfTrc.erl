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
-export([loop/1]).

-import(lists,[reverse/1,foreach/2,map/2]).
-import(dict,[new/0,store/3,fetch/2,from_list/1]).

-define(LOOP, ?MODULE:loop).
-define(LOG(T), prf:log(process_info(self()),T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prfTarg process

collect(LD) -> {LD, {?MODULE, {tick,now()}}}.

config(LD,{start,Conf}) -> start(Conf),LD;
config(LD,{stop,Args}) -> stop(Args),LD;
config(LD,Data) -> ?LOG([unknown,{data,Data}]), LD.

start(Conf) ->
  assert(prfTrc) ! {start, Conf}.

stop(Args) ->
  assert(prfTrc) ! {stop,Args}.

assert(Reg) ->
  case whereis(Reg) of
    Pid when is_pid(Pid) -> Pid;
    undefined -> register(Reg,Pid=spawn(fun init/0)),Pid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace control process
%%% LD = idle | {host_pid,timer,consumer,conf}
%%% Conf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,Pid,Count} | {term_stream,Pid,Count} |
%%%         {file,File,Size} | {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  process_flag(trap_exit,true),
  loop(idle).

loop(LD) ->
  receive
    {start,{HostPid,Conf}} ->
      ?LOOP(start_trace(HostPid,LD,Conf));
    {stop,{HostPid,Args}} -> 
      ?LOOP(stop_trace(HostPid,LD,Args));
    {timeout,_Timer,{die,HostPid}} -> 
      ?LOOP(stop_trace(HostPid,LD,{timeout}));
    {'EXIT',Pid,R} ->
      ?LOOP(handle_exit(Pid,R,LD))
  end.

handle_exit(_Pid,normal,idle) ->
  idle;
handle_exit(Pid,R,LD) ->
  case {fetch(consumer,LD),fetch(host_pid,LD)} of
    {_,Pid} -> stop_trace(Pid,LD,{host_died,Pid});
    {Pid,HostPid} -> stop_trace(HostPid,LD,{consumer_died,R});
    X -> ?LOG({wierd_exit,X,R}), LD
  end.

stop_trace(HostPid,idle,_Args) ->
  HostPid ! {prfTrc,{not_started,self()}},
  idle;
stop_trace(HostPid,LD,Args) ->
  HostPid ! {prfTrc,{stopping,self(),Args}},
  unlink(fetch(host_pid,LD)),
  erlang:cancel_timer(fetch(timer,LD)),
  erlang:trace(all,false,fetch(flags,fetch(conf,LD))),
  unset_tps(),
  consumer_stop(fetch(consumer,LD)),
  idle.

start_trace(HostPid,idle,Conf) ->
  HostPid ! {prfTrc,{starting,self()}},
  link(HostPid),
  unset_tps(),
  Cons = consumer(fetch(where,Conf)),
  Procs = mk_prc(fetch(procs,Conf)),
  Flags = [{tracer,Cons}|fetch(flags,Conf)],
  erlang:trace(Procs,true,Flags),
  set_tps(fetch(rtps,Conf)),
  Timer = erlang:start_timer(fetch(time,Conf),self(),{die,HostPid}),
  from_list([{host_pid,HostPid},{timer,Timer},{consumer,Cons},{conf,Conf}]);
start_trace(HostPid,LD,_Conf) ->
  HostPid ! {prfTrc,{already_started,self()}},
  LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consumer({term_buffer,Pid,Count}) -> consumer_pid(Pid,Count,yes);
consumer({term_stream,Pid,Count}) -> consumer_pid(Pid,Count,no);
consumer({file,File,Size}) -> consumer_file(File,Size);
consumer({ip,Port,Queue}) -> consumer_ip(Port,Queue).

consumer_stop(Pid) when is_pid(Pid) -> Pid ! stop;
consumer_stop(_Port) when is_port(_Port) -> dbg:flush_trace_port().

consumer_pid(Pid,Cnt,Buf) ->
  Conf = from_list([{daddy,self()},{count,Cnt},
                    {maxsize,50000},{maxqueue,100},
                    {where,Pid},{buffering,Buf}]),
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

set_tps_f({MFA,MS,Fs}) -> erlang:trace_pattern(MFA,MS,Fs).


mk_prc(all) -> all;
mk_prc(Pid) when pid(Pid) -> Pid;
mk_prc({pid,P1,P2}) when integer(P1), integer(P2) -> c:pid(0,P1,P2);
mk_prc(Reg) when atom(Reg) -> 
  case whereis(Reg) of 
    undefined -> exit({no_such_process, Reg});
    Pid when pid(Pid) -> Pid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  the local consumer process. 
%%%  buffers trace messages, and flushes them when;
%%%    it gets a stop from the controller
%%%    reaches count=0
%%%    a stacktrace is too big
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
-record(ld,{daddy,where,count,maxqueue,maxsize}).

init_local(Conf) -> lloop({#ld{daddy=fetch(daddy,Conf),
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
    stop -> flush(LD,Buff);
    {trace_ts,Pid,Tag,A,TS} -> lloop(msg(LD,Buff,Count,{Tag,Pid,TS,A}));
    {trace_ts,Pid,Tag,A,B,TS}->lloop(msg(LD,Buff,Count,{Tag,Pid,TS,{A,B}}))
  end.

msg(LD,Buff,Count,Item) ->
  maybe_exit(msg_size,{LD,Item}),
  {LD,buff(Buff,LD,Item),Count-1}.

buff(no,LD,Item) -> send_one(LD,Item),no;
buff(Buff,_LD,Item) -> [Item|Buff].

maybe_exit(msg_count,{LD,Buff,0}) -> flush(LD,Buff), exit({msg_count});
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
flush(#ld{where=Pid},Buffer) -> Pid ! map(fun msg/1, reverse(Buffer)).

msg({'send',Pid,TS,{Msg,To}}) ->       {'send',{Msg,pi(To)},pi(Pid),ts(TS)};
msg({'receive',Pid,TS,Msg}) ->         {'recv',Msg,         pi(Pid),ts(TS)};
msg({'return_from',Pid,TS,{MFA,V}}) -> {'retn',{MFA,V},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,{MFA,B}}) ->        {'call',{MFA,B},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,MFA}) ->            {'call',{MFA,<<>>},  pi(Pid),ts(TS)}.

pi(P) when pid(P) ->
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
pi(P) when port(P) -> 
  {name,N} = erlang:port_info(P,name),
  [Hd|_] = string:tokens(N," "),
  reverse(hd(string:tokens(reverse(Hd),"/")));
pi(R) when atom(R) -> R;
pi({R,Node}) when atom(R), Node == node() -> R;
pi({R, Node}) when atom(R), atom(Node) -> {R, Node}.

ts(Nw) ->
  {_,{H,M,S}} = calendar:now_to_local_time(Nw),
  {H,M,S,element(3,Nw)}.
