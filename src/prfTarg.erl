%%%-------------------------------------------------------------------
%%% File    : prfTarg.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : 
%%%
%%% Created :  1 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfTarg).

-export([subscribe/3,unsubscribe/2]).
-export([init/0,loop/1]).		 %internal; otp r5 compatible!

-import(net_kernel,[get_net_ticktime/0]).
-import(orddict,[new/0,store/3,fold/3,map/2,find/2]).

-record(st, {collectors=new()}).
-record(collector, {subscribers=[],state=init}).

-define(LOG(T), prf:log(process_info(self()),T)).
%%% interface %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subscribe(Node, Pid, Collectors) -> 
    {PID,Tick} = assert(Node,Collectors),
    link(PID), 
    PID ! {subscribe, {Pid,Collectors}}, 
    {PID,Tick}.

unsubscribe(Node, Pid) -> 
    {Node,?MODULE} ! {unsubscribe, {Pid}}.

%%% runs on host %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert(Node,Collectors) ->
    assert_loaded(Node,Collectors), 
    assert_started(Node).

assert_loaded(Node, Collectors) ->
    lists:foreach(fun(M) -> ass_loaded(Node,M) end, [prf,?MODULE|Collectors]).

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
		{ack, Pid, Tick} -> erlang:demonitor(Ref), {Pid,Tick};
		{'DOWN', Ref, process, Pid, {use_me, PID,Tick}} -> {PID,Tick}
	    after 
		3000 -> exit(timeout)
	    end
    end.

%%% runs on target %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    erlang:process_flag(trap_exit,true),
    erlang:group_leader(whereis(user),self()),
    receive {start,Pid} -> ok end,
    case whereis(?MODULE) of   %to avoid register error msg...
	undefined -> 
	    case catch register(?MODULE, self()) of
		{'EXIT', {badarg, _}} ->       %somebody beat us to it
		    exit({use_me, whereis(?MODULE),get_net_ticktime()});
		true -> 
		    Pid ! {ack, self(),get_net_ticktime()},
                    prf:ticker_odd(),
		    loop(#st{})
	    end;
	PID ->
	    exit({use_me, PID,get_net_ticktime()})
    end.

loop(St) ->
    receive
	{timeout, _, {tick}} -> 
	    prf:ticker_odd(),
	    ?MODULE:loop(collect_and_send(St));
	{subscribe, {Pid,Collectors}} -> 
	    ?MODULE:loop(subscr(St, Pid, Collectors));
	{unsubscribe, {Pid}} -> 
	    ?MODULE:loop(unsubscr(St, Pid));
	{'EXIT', Pid, _} ->
	    ?MODULE:loop(unsubscr(St, Pid));
        {config,Data} ->
            ?MODULE:loop(config(St,Data));
	dbg ->
	    F = fun(M,#collector{subscribers=S},A) -> [{mod,M},{subsc,S}|A] end,
	    ?LOG([{pid,self()} | fold(F,[],St#st.collectors)]),
	    ?MODULE:loop(St);
	stop -> 
            ok
    end.

config(St,Data) ->
    St#st{collectors=map(fun(K,V)->conf(K,V,Data) end,St#st.collectors)}.

conf(C,Collector,Data) ->
    State = C:config(Collector#collector.state,Data),
    Collector#collector{state=State}.

subscr(St, Pid, Cs) ->
    link(Pid),
    {Pid,Collectors} = lists:foldl(fun subs/2, {Pid,St#st.collectors}, Cs),
    St#st{collectors=Collectors}.

subs(C, {Pid, Collectors}) -> 
    {Pid,store(C,collector(Collectors,Pid,C),Collectors)}.

collector(Colls,Pid,C) ->
    case find(C, Colls) of
	{ok, Coll = #collector{subscribers=Subs}} ->
	    Coll#collector{subscribers=lists:umerge(Subs,[Pid])};
	error -> 
	    #collector{subscribers=[Pid]}
    end.

unsubscr(St = #st{collectors = Colls}, Pid) ->
    St#st{collectors=map(fun(_K,V)->unsubs(Pid,V) end, Colls)}.

unsubs(Pid,C) -> 
    C#collector{subscribers=C#collector.subscribers--[Pid]}.


collect_and_send(St) ->
    {Data,Colls} = fold(fun cns/3, {[],St#st.collectors},St#st.collectors),
    send(regroup(Data)),
    St#st{collectors=Colls}.

cns(C,Coll,{Datas,Colls}) ->
    #collector{state=State, subscribers=Subs} = Coll,
    {NState, Data} = (C):collect(State),
    {[{Data,Subs}|Datas], store(C,Coll#collector{state=NState},Colls)}.

regroup(Data) -> lists:foldl(fun regr/2,new(),Data).

regr({Data,Subs},Dict) ->
    lists:foldl(fun(S,T) -> regrr(S,T,Data) end, Dict, Subs).

regrr(Sub,Dict,Data) ->
    case find(Sub,Dict) of
	{ok,Datas} -> store(Sub,[Data|Datas],Dict);
	error -> store(Sub,[Data],Dict)
    end.

send(Dict) -> fold(fun sendf/3, [], Dict).

sendf(Sub,Data,_) -> Sub ! {{data,node()},Data}.
