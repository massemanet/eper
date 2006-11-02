%%%-------------------------------------------------------------------
%%% File    : prfTarg.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  1 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfTarg).

-export([subscribe/3,unsubscribe/3]).
-export([init/0,loop/1]).		 %internal; otp r5 compatible!
%%-export([netload/2]).

-import(gb_trees,[empty/0,insert/3,iterator/1,
		  lookup/2,next/1,update/3,to_list/1]).

-record(st, {collectors=empty()}).
-record(collector, {subscribers=[],mod=[],state=init}).

-include("prf.hrl").

%%% interface %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subscribe(Node, Pid, Collectors) -> 
    PID = assert(Node,Collectors),
    link(PID), 
    PID ! {subscribe, {Pid,Collectors}}, 
    PID.
unsubscribe(Node, Pid, Collectors) -> 
    {Node,?MODULE} ! {unsubscribe, {Pid,Collectors}}.

%%% runs on host %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert(Node,Collector) ->
    assert_loaded(Node,Collector), 
    assert_started(Node).

assert_loaded(Node, Collectors) ->
    lists:foreach(fun(M) -> ass_loaded(Node,M) end, [?MODULE|Collectors]).

ass_loaded(Node, Mod) ->
    case rpc:call(Node,Mod,module_info,[compile]) of
	{badrpc,{'EXIT',{undef,_}}} -> 		%no code
	    netload(Node, Mod),
	    ass_loaded(Node, Mod);
	{badrpc,_} ->
	    ok;
	CompInfo when list(CompInfo) ->
	    case {ftime(CompInfo), ftime(Mod:module_info(compile))} of
		{interpreted,_} ->
		    ok;
		{TargT, HostT} when TargT < HostT -> %old code on target
		    netload(Node, Mod),
		    ass_loaded(Node, Mod);
		_ -> 
		    ok
	    end
    end.

netload(Node, Mod) ->
    {Mod, Bin, Fname} = code:get_object_code(Mod),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, Fname, Bin]).

ftime([]) -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T]) -> ftime(T).

assert_started(Node) ->
    case net_adm:ping(Node) of
	pang->
	    exit(node_down);
	pong ->
	    Pid = spawn(Node, ?MODULE, init, []),
	    Ref = erlang:monitor(process, Pid),
	    Pid ! {start,self()},
	    receive 
		{ack, Pid} -> erlang:demonitor(Ref), Pid;
		{'DOWN', Ref, process, Pid, {use_me, PID}} -> PID
	    after 
		?TICK -> exit(timeout)

	    end
    end.

%%% runs on target %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    erlang:process_flag(trap_exit,true),
    receive {start,Pid} -> ok end,
    case whereis(?MODULE) of   %to avoid register error msg...
	undefined -> 
	    case catch register(?MODULE, self()) of
		{'EXIT', {badarg, _}} ->       %somebody beat us to it
		    exit({use_me, whereis(?MODULE)});
		true -> 
		    Pid ! {ack, self()},
		    erlang:start_timer(incr(?TICK), self(), tick),
		    loop(#st{})
	    end;
	PID ->
	    exit({use_me, PID})
    end.

loop(St) ->
    receive
	{timeout, _, tick} -> 
	    erlang:start_timer(incr(?TICK), self(), tick),
	    ?MODULE:loop(collect_and_send(St));
	{subscribe, {Pid,Collectors}} -> 
	    ?MODULE:loop(subscr(St, Pid, Collectors));
	{unsubscribe, {Pid,Collectors}} -> 
	    ?MODULE:loop(unsubscr(St, Pid, Collectors));
	{'EXIT', Pid, _} ->
	    ?MODULE:loop(unsubscr(St, Pid, all));
	dbg ->
	    L = [Val || {_Key,Val} <- to_list(St#st.collectors)],
	    X = [[{mod,M},{subsc,S}] || #collector{subscribers=S,mod=M} <- L],
	    ?LOG([{pid,self()}|X]),
	    ?MODULE:loop(St);
	stop -> ok
    end.

subscr(St = #st{collectors=CollTree}, Pid, Colls) ->
    link(Pid),
    St#st{collectors=subsc(CollTree, Pid, Colls)}.

subsc(CollTree, _Pid, []) -> CollTree;
subsc(CollTree, Pid, [Coll|Colls]) ->
    case lookup(Coll, CollTree) of
	{value, C = #collector{subscribers=Subs}} ->
	    Val = C#collector{subscribers=[Pid|(Subs--[Pid])],mod=Coll},
	    subsc(update(Coll,Val,CollTree),Pid,Colls);
	none -> 
	    Val = #collector{subscribers=[Pid],mod=Coll},
	    subsc(insert(Coll,Val,CollTree),Pid,Colls)
    end.

unsubscr(St= #st{collectors = CollTree}, Pid, all) ->
    unsubscr(St, Pid, get_colls(CollTree,Pid));
unsubscr(St= #st{collectors = CollTree}, Pid, Colls) ->
    St#st{collectors=unsubsc(CollTree,Pid,Colls)}.

unsubsc(CollTree,_Pid,[]) -> CollTree;
unsubsc(CollTree,Pid,[Coll|Colls]) -> 
    {value,C = #collector{subscribers=Subs}} = lookup(Coll,CollTree),
    Val = C#collector{subscribers=Subs--[Pid]},
    unsubsc(update(Coll,Val,CollTree),Pid,Colls).

get_colls(CollTree,Pid) ->
    get_colls(Pid,next(iterator(CollTree)),[]).

get_colls(_Pid,none,O) -> O;
get_colls(Pid,{_Key,#collector{subscribers=Ss,mod=Mod},Iter},O) ->
    case lists:member(Pid,Ss) of
	true -> get_colls(Pid,next(Iter),[Mod|O]);
	false -> get_colls(Pid,next(Iter),O)
    end.

collect_and_send(St = #st{collectors=CollTree}) ->
    St#st{collectors=cns(next(iterator(CollTree)),CollTree,[])}.

cns(none,Tree,Data) -> 
    send(regroup(Data)),
    Tree;
cns({_Key,#collector{subscribers=[]},Iter},Tree,O) ->
    cns(next(Iter),Tree,O);
cns({Key,C = #collector{subscribers=Ss},Iter},Tree,O) ->
    {NState, Data} = (C#collector.mod):collect(C#collector.state),
    cns(next(Iter),update(Key,C#collector{state=NState},Tree),[{Data,Ss}|O]).

regroup(Data) -> lists:foldl(fun regr/2,empty(),Data).

regr({Data,Subs},Tree) ->
    lists:foldl(fun(S,T) -> regrr(S,T,Data) end, Tree, Subs).

regrr(Sub,Tree,Data) ->
    case lookup(Sub,Tree) of
	none -> insert(Sub,[Data],Tree);
	{value,Datas} -> update(Sub,[Data|Datas],Tree)
    end.

send(Tree) -> lists:foreach(fun sendf/1, to_list(Tree)).

sendf({Sub,Data}) -> Sub ! {{data,node()},Data}.

incr(Tick) ->
    {_, Sec, Usec} = now(),
    Tick+500-(round(Sec*1000+Usec/1000+500) rem Tick).
