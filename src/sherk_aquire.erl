%%%-------------------------------------------------------------------
%%% File     : sherk_aquire.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description : 
%%%
%%% Created : 16 Aug 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(sherk_aquire).

-export([go/6]).
-export([stop/0,kill/0]).
-export([check_dir/1]).
-export([ass_loaded/2]).

-include_lib("kernel/include/file.hrl").

-import(dict,[from_list/1,fetch/2,store/3]).
-import(lists,[foldl/3,map/2,member/2]).

-define(LOG(T), sherk:log(process_info(self()),T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the API
%%%
%%% Eg;
%%% sherk_aquire:go(1000,[call,timestamp],[{'_','_'}],all,[mwux005@mwux005],foo,{file,"/tmp/sherk/gruff",0,"/tmp"}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go(Time,Flags,RTPs,Procs,Targs,Dest) -> 
    check_and_spawn(Time,Flags,RTPs,Procs,Targs,Dest).

stop() -> catch (sherk_host ! stop).

kill() -> catch exit(erlang:whereis(sherk_host),kill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% most argument checking is done here. some pid-related checking has 
%% to be deferred to the target 

check_and_spawn(Time,Flags,RTPs,Procs,Targs,Dest) ->
    LD = from_list([{time,chk_time(Time)},
		    {flags,chk_flags(Flags)},
		    {rtps,chk_rtps(RTPs)},
		    {procs,chk_procs(Procs)},
		    {dest,chk_dest(Dest)},
		    {targs,chk_conns(Targs)},
		    {daddy,self()}]),
    
    (Pid = spawn(fun init/0)) ! {init,LD},
    Pid.

chk_conns(Targs) -> map(fun(T)->chk_conn(T) end,Targs).

chk_conn(T) when T==node() -> T;
chk_conn(T) -> 
    case net_adm:ping(T) of
	pong -> ass_loaded(T,sherk_target);
	pang -> exit({connection_failed,T})
    end.

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_procs(X) when all==X; existing==X; new==X -> [X];
chk_procs(Ps) when is_list(Ps) -> map(fun chk_proc/1, Ps);
chk_procs(X) -> exit({bad_proc_spec,X}).

chk_proc(X) when X==all; X==existing; X==new -> exit({not_allowed,X});
chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom)-> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1), is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> exit({bad_proc,X}).

chk_dest({ip,P,S}) when is_integer(P),is_integer(S) ->{ip,{P,S}};
chk_dest({file,F,S,T}) when $/==hd(F),is_integer(S),$/==hd(T) ->{file,{F,S,T}};
chk_dest(X) -> exit({bad_dest,X}).

chk_flags(Fs) -> map(fun chk_flag/1, Fs).

chk_flag(F) -> 
    case member(F,trace_flags()) of 
	true -> F;
	false -> exit({bad_flag,F})
    end.

chk_rtps(RTPs) -> map(fun chk_rtp/1, RTPs).

chk_rtp({M,F}) when atom(M), atom(F) -> {{M,F,'_'},[],[local]};
chk_rtp({M,F,MS}) when atom(M), atom(F) -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X) -> exit({bad_rtp,X}).

ms(MS) -> foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack, [{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return, [{Head,Cond,Body}]) -> [{Head,Cond,[{return_trace}|Body]}];
msf(Head, [{_,Cond,Body}]) when tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> exit({bad_match_spec,X}).

trace_flags() ->
    ['send','receive','procs','call','return_to','running',
     'garbage_collection','timestamp','cpu_timestamp','arity','set_on_spawn'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the host process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    sherk_target:self_register(sherk_host),
    process_flag(trap_exit,true),
    receive
	{init,LD} -> 
	    Targs = fetch(targs,LD),
	    Pids = [spawn_link(T, fun sherk_target:init/0) || T <- Targs],
	    [ P ! {init,store(daddy,self(),LD)} || P <- Pids],
	    Timer = erlang:start_timer(fetch(time,LD),self(),{die}),
	    loop(store(pids,Pids,store(timer,Timer,LD)))
    end.

loop(LD) ->
    receive
	{timeout,_,{die}} -> 
	    ?LOG({timed_out}),
	    stop(LD);
	stop -> 
	    stop(LD);
	{'EXIT',P,R} -> 
	    ?LOG([got_exit,{from,node(P)},{reason,R}]),
	    case fetch(pids,LD) of
		[P] -> ?LOG(all_clients_dead);
		Ps -> loop(store(pids,Ps--[P],LD))
	    end
    end.

stop(LD) -> 
    Pids = fetch(pids,LD),
    [P ! stop || P <- Pids],
    recv(Pids,fetch(dest,LD),dict:new()).

recv(_,{ip,_},_) -> ok;
recv(Pids,{file,{Dir,_,_}},FDs) -> recv(Pids,Dir,FDs);
recv([],_,FDs) -> 
    case dict:fold(fun(P,_,A)->[node(P)|A] end,[],FDs) of
	[] -> ok;
	X -> ?LOG({fds_still_open,X})
    end;
recv(Pids,Dir,FDs) ->
    receive
	{'EXIT',P,R} 	    -> recv(bye(P,R,Pids),Dir,close(P,FDs));
	{P,chunk,eof} 	    -> recv(bye(P,eof,Pids),Dir,close(P,FDs));
	{P,chunk,{error,R}} -> recv(bye(P,R,Pids),Dir,close(P,FDs));
	{P,chunk,B} when is_binary(B) -> recv(Pids,Dir,stuff(P,B,Dir,FDs))
    end.

stuff(P,B,Dir,FDs) ->
    try fetch(P,FDs) of
	FD -> 
	    file:write(FD,B),
	    FDs
    catch
	_:_ -> 
	    File = filename:join(Dir,node(P))++".trz",
	    filelib:ensure_dir(File),
	    {ok,FD} = file:open(File,[raw,write,compressed]),
	    ?LOG({opened,File}),
	    stuff(P,B,Dir,store(P,FD,FDs))
    end.

close(P,FDs) ->
    try fetch(P,FDs) of
	FD -> 
	    file:close(FD),
	    dict:erase(P,FDs)
    catch
	_:_ -> FDs
    end.

bye(P,R,Pids) ->
    ?LOG([{client_finished,node(P)},{reason,R}]),
    Pids--[P].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ass_loaded(Node, Mod) ->
    case rpc:call(Node,Mod,module_info,[compile]) of
	{badrpc,{'EXIT',{undef,_}}} -> 		%no code
	    netload(Node, Mod),
	    ass_loaded(Node, Mod);
	{badrpc,_} ->
	    exit({no_connection,Node});
	CompInfo when list(CompInfo) ->
	    case {ftime(CompInfo), ftime(Mod:module_info(compile))} of
		{interpreted,_} ->
		    exit({target_has_interpreted_code,Mod});
		{TargT, HostT} when TargT < HostT -> %old code on target
		    netload(Node, Mod),
		    ass_loaded(Node, Mod);
		_ -> 
		    Node
	    end
    end.

netload(Node, Mod) ->
    {Mod, Bin, Fname} = code:get_object_code(Mod),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, Fname, Bin]).

ftime([]) -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T]) -> ftime(T).

check_dir(Dir) ->
    {ok,#file_info{type=directory, access=read_write}} = 
        file:read_file_info(Dir).

