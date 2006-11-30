%%%-------------------------------------------------------------------
%%% File     : redbug.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description : 
%%%
%%% Created : 16 Aug 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(redbug).

-import(lists,[foreach/2,foldl/3,reverse/1,map/2]).

-export([help/0]).
-export([go/3,go/4,go/5,go/6]).
-export([stop/0,stop/1,kill/0,kill/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help() ->
    strs(["redbug:go(Time,Msgs,Trc[,Proc,[Where,[Targ]]])",
	  "Time: integer() [ms]",
	  "Msgs: integer() [#]",
	  "Trc: list('send'|'receive'|{M,F}|{M,F,RestrictedMatchSpec})",
	  "Proc: 'all'|pid()|atom(Regname)|{'pid',I2,I3}",
	  "Where: 'screen'|'term'|{'file',Filename}|{'ip',Port}",
	  "Targ: node()"]).

go(Time,Msgs,Trc) -> go(Time,Msgs,Trc,all).

go(Time,Msgs,Trc,Proc) -> go(Time,Msgs,Trc,Proc,screen).

go(Time,Msgs,Trc,Proc,Where) -> go(Time,Msgs,Trc,Proc,Where,node()).

go(Time,Msgs,Trc,Proc,Where,Targ)  ->
    check_and_spawn(Time,Msgs,Trc,Proc,Where,Targ).

stop() -> stop(node()).
stop(Node) ->
    case rpc:call(Node,erlang,whereis,[?MODULE]) of
	{badrpc,nodedown} -> not_running;
	undefined -> not_running;
	Pid -> Pid ! stop
    end.

kill() -> kill(node()).
kill(Node) ->
    case  rpc:call(Node,erlang,whereis,[?MODULE]) of
	{badrpc,nodedown} -> not_running;
	undefined -> not_running;
	Pid -> exit(Pid,kill)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% most argument checking is done here. some pid-related checking has 
%% to be deferred to the target 

check_and_spawn(Time,Msgs,Trc,Proc,Where,Targ) ->
    {Flags,RTPs} = foldl(fun chk_trc/2,{[],[]},ass_list(Trc)),
    cross_chk(Proc,Flags),
    Conf = [{time,chk_time(Time)},
	    {msgs,chk_msgs(Msgs)},
	    {flags,[call,timestamp|Flags]},
	    {rtps,RTPs},
	    {procs,chk_proc(Proc)},
	    {where,chk_where(Where)},
	    {daddy,self()}],
    case net_adm:ping(Targ) of
	pong -> Pid = spawn_link(Targ, fun init/0) ! {init,Conf}, Pid;
	pang -> exit({no_such_node,Targ})
    end.

cross_chk(all,Flags) when 0<length(Flags) -> exit({too_many_procs,{all,Flags}});
cross_chk(_,_) -> ok.

ass_list(L) when is_list(L) -> lists:usort(L);
ass_list(X) -> [X].

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

chk_where(term) -> {term,self()};
chk_where(screen) -> screen;
chk_where({ip,Port}) when is_integer(Port) -> {ip,Port};
chk_where({file,File}) when $/ == hd(File) -> {file,File};
chk_where(X) -> exit({bad_where,X}).

chk_rtp({M,F}) when atom(M), atom(F), M/='_' -> {{M,F,'_'},[],[local]};
chk_rtp({M,F,MS}) when atom(M), atom(F), M/='_' -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X) -> exit({bad_rtp,X}).

ms(MS) -> foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack, [{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return, [{Head,Cond,Body}]) -> [{Head,Cond,[{return_trace}|Body]}];
msf(Head, [{_,Cond,Body}]) when tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> exit({bad_match_spec,X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  the control process
%%%  tracing can go to a file, or to a local consumer process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(ld,{daddy,time,msgs,flags,rtps,procs,where,timer,consumer}).

init() ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) -> 
	    exit({already_running});
	undefined -> 
	    register(?MODULE,self()),
	    process_flag(trap_exit,true),
	    receive
		{init,Conf} -> loop(start(check_ld(conf(#ld{},Conf))))
	    end
    end.

loop(LD) ->
    receive
	{timeout,_,{die}} -> ok;
	stop -> ok;
	{'EXIT',R} -> io:fwrite("~p: exit - ~p~n", [?MODULE, R])
    end,
    LD#ld.daddy ! stop,
    stop_trace(LD).

start(LD) ->
    unset_tps(),
    Cons = consumer(LD#ld.where,LD#ld.msgs),
    start_trace(mk_prc(LD#ld.procs),[{tracer,Cons}|LD#ld.flags]),
    set_tps(LD#ld.rtps),
    Timer = erlang:start_timer(LD#ld.time,self(),{die}),
    LD#ld{timer=Timer,consumer=Cons}.

check_ld(LD) ->
    case mk_prc(LD#ld.procs) of
	all -> P = new;
	Pid when is_pid(Pid) -> P = Pid
    end,
    case erlang:trace_info(P,tracer) of
	{tracer,[]} -> LD;
	{tracer,Tracer} ->
	    if is_pid(Tracer) -> I = process_info(Tracer);
	       is_port(Tracer) -> I = erlang:port_info(Tracer)
	    end,
	    exit({already_traced,I})
    end.

consumer(screen,Count) -> consumer_local(screen,Count);
consumer({term,Pid},Count) -> consumer_local(Pid,Count);
consumer({file,File},Size) -> consumer_file(File,Size);
consumer({ip,Port}, _) -> consumer_ip(Port).

consumer_stop(Pid) when is_pid(Pid) -> Pid ! stop;
consumer_stop(_Port) when is_port(_Port) -> dbg:flush_trace_port().

consumer_file(File,Size) ->
    %% number of files
    WrapCnt = 2,
    %% file size (per file). Size is given in Mb.
    WrapSize = Size*1024*1024,
    Suffix = ".trc",
    (dbg:trace_port(file,{File, wrap, Suffix, WrapSize, WrapCnt}))().

consumer_ip(Port) ->
    %% keep at most this many in the buffer on the sender side
    QueSize = 128, 
    (dbg:trace_port(ip,{Port, QueSize}))().

consumer_local(Where,Count) ->
    Conf = [{daddy,self()},{count,Count},{where,Where}],
    spawn_link(fun() -> init_local(Conf) end).

start_trace(Proc,Flags) -> erlang:trace(Proc,true,Flags).

stop_trace(LD) ->
    erlang:trace(all,false,LD#ld.flags),
    unset_tps(),
    consumer_stop(LD#ld.consumer).

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
%%%  flushing can be done by printing or by sending a term to 'where'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(lld,{daddy,where,count=0,maxqueue,maxsize,buffer=[]}).

init_local(Conf) -> lloop(conf(#lld{},Conf)).

lloop(LD = #lld{count=0}) -> 
    LD#lld.daddy ! stop,
    flush(LD);
lloop(LD) ->
    maybe_exit(LD),
    receive 
	stop -> flush(LD);
	{trace_ts,Pid,Tag,A,TS} -> lloop(msg(LD,{Tag,Pid,TS,A}));
	{trace_ts,Pid,Tag,A,B,TS} -> lloop(msg(LD,{Tag,Pid,TS,{A,B}}))
    end.

msg(LD = #lld{count=Count,buffer=Buffer}, Item) ->
    maybe_exit(LD,Item),
    LD#lld{count=Count-1,buffer=[Item|Buffer]}.

maybe_exit(LD) ->
    case process_info(self(),message_queue_len) of
	{_,N} when N > LD#lld.maxqueue -> exit({msg_queue,N});
	_ -> ok
    end.

maybe_exit(LD,Item) -> 
    case Item of
	{call,_Pid,_TS,{_A,B}} when is_binary(B),LD#lld.maxsize<size(B) ->
	    exit({stack_size,size(B)});
	_ -> 
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% we format trace messages and either print them or send them 

flush(#lld{where=screen,buffer=Buffer}) -> flush_screen(Buffer);
flush(#lld{where=Pid,buffer=Buffer}) -> flush_term(Buffer,Pid).

flush_term(Buffer,Pid) ->
    Pid ! map(fun outer_t/1, reverse(Buffer)).

outer_t(Msg) ->
    case msg(Msg) of
	{call,{MFA,Bin},PI,TS} ->
	    {call,{MFA,stak(Bin)},PI,TS};
	MSG -> 
	    MSG
    end.

flush_screen(Buffer) ->
    io:fwrite("~n**~p stopped - ~p msgs**~n",[?MODULE, length(Buffer)]),
    foreach(fun outer_w/1, reverse(Buffer)).

outer_w(Msg) ->
    case msg(Msg) of
	{call,{MFA,Bin},PI,TS} when is_binary(Bin) ->
	    io:fwrite("~p~n",[{call,MFA,PI,TS}]),
	    foreach(fun(L)->io:fwrite("  ~p~n",[L]) end, stak(Bin));
	MSG -> 
	    io:fwrite("~p~n", [MSG])
    end.

msg({'send',Pid,TS,{Msg,To}}) ->         {'send',{Msg,pi(To)},pi(Pid),ts(TS)};
msg({'receive',Pid,TS,Msg}) ->           {'receive',Msg,pi(Pid),ts(TS)};
msg({'return_from',_Pid,_TS,{MFA,V}}) -> {'return',{MFA,V}};
msg({'call',Pid,TS,{MFA,B}}) ->          {'call',{MFA,B},pi(Pid),ts(TS)};
msg({'call',Pid,TS,MFA}) ->              {'call',{MFA,<<>>},pi(Pid),ts(TS)}.

pi(P) when pid(P) ->
    case process_info(P, registered_name) of
	[] -> 
	    case process_info(P, initial_call) of
		{_, {proc_lib,init_p,5}} -> proc_lib:translate_initial_call(P);
		{_,MFA} -> MFA;
		undefined -> dead
	    end;
	{_,Nam} -> Nam;
	undefined -> dead
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	    case catch {list_to_atom(M),list_to_atom(F),list_to_integer(A)} of
		{'EXIT',_} -> C;
		X -> X
	    end;
	["unknown","function"] ->
	    unknown_function
    end.

strs([]) -> ok;
strs([H|T]) -> io:fwrite("~s~n",[H]),strs(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% loop data utilities
%%% records truly are the mother of all kludges...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fields(lld) -> record_info(fields,lld);
fields(ld) -> record_info(fields,ld).

conf(Rec,Conf) -> 
    Fun = fun(F,{A,N}) -> {[{F,N+2}|A], N+1} end,
    {Fs,_L} = foldl(Fun,{[],0},fields(element(1,Rec))),
    conf(Conf,Fs,Rec).

conf([], _Fields, LD) -> LD;
conf([{Field,Val}|Conf], Fields, LD) ->
    case lists:keysearch(Field,1,Fields) of
	{value,{Field,Pos}} -> conf(Conf,Fields,setelement(Pos,LD,Val));
	false -> exit({bad_conf,{Field,Val}})
    end.
