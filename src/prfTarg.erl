%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Created :  1 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfTarg).

-export([config/1,subscribe/3,unsubscribe/2]).
-export([init/0,loop/1]).                %internal; otp r5 compatible!

-record(st,{collectors=orddict:new()}).

-record(collector,{subscribers=[],
                    state=init}).

-include("log.hrl").

%%% interface %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(ConfData) ->
  case whereis(prfTarg) of
    undefined -> not_started;
    Pid -> Pid ! {config,ConfData}
  end.

subscribe(Node,Pid,Collectors) ->
  {PID,Tick} = assert(Node,Collectors),
  PID ! {subscribe,{Pid,Collectors}},
  {PID,Tick}.

unsubscribe(Node,Pid) ->
  {Node,?MODULE} ! {unsubscribe,{Pid}}.

%%% runs on host %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert(Node,Collectors) ->
  assert_loaded(Node,Collectors),
  assert_started(Node).

assert_loaded(Node,Collectors) ->
  lists:foreach(fun(M) -> ass_loaded(Node,M) end,
                [prf,prf_crypto,?MODULE|Collectors]).

ass_loaded(nonode@nohost,Mod) -> {module,Mod}=c:l(Mod);
ass_loaded(Node,Mod) ->
  case rpc:call(Node,Mod,module_info,[compile]) of
    {badrpc,{'EXIT',{undef,_}}} ->              %no code
      netload(Node,Mod),
      ass_loaded(Node,Mod);
    {badrpc,_} ->
      ok;
    CompInfo when is_list(CompInfo) ->
      case {ftime(CompInfo),ftime(Mod:module_info(compile))} of
        {interpreted,_} ->
          ok;
        {TargT,HostT} when TargT < HostT -> %old code on target
          netload(Node,Mod),
          ass_loaded(Node,Mod);
        _ ->
          ok
      end
  end.

netload(Node,Mod) ->
  {Mod,Bin,Fname} = code:get_object_code(Mod),
  {module,Mod} = rpc:call(Node,code,load_binary,[Mod,Fname,Bin]).

ftime([]) -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T]) -> ftime(T).

assert_started(nonode@nohost) ->
  starter(spawn(?MODULE,init,[]));
assert_started(Node) ->
  case net_adm:ping(Node) of
    pang->
      exit(node_down);
    pong ->
      starter(spawn(Node,?MODULE,init,[]))
  end.

starter(Pid) ->
  Ref = erlang:monitor(process,Pid),
  Pid ! {start,self()},
  receive
    {ack,Pid,Tick} -> erlang:demonitor(Ref),{Pid,Tick};
    {'DOWN',Ref,process,Pid,{use_me,PID,Tick}} -> {PID,Tick}
  end.

%%% runs on target %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  erlang:process_flag(trap_exit,true),
  erlang:group_leader(whereis(user),self()),
  Pid = receive {start,P} -> P end,
  case whereis(?MODULE) of   %to avoid register error msg...
    undefined ->
      case catch register(?MODULE,self()) of
        {'EXIT',{badarg,_}} ->       %somebody beat us to it
          exit({use_me,whereis(?MODULE),net_kernel:get_net_ticktime()});
        true ->
          Pid ! {ack,self(),net_kernel:get_net_ticktime()},
          prf:ticker_odd(),
          loop(#st{})
      end;
    PID ->
      exit({use_me,PID,net_kernel:get_net_ticktime()})
  end.

loop(St) ->
  receive
    {timeout,_,{tick}} ->
      prf:ticker_odd(),
      erlang:garbage_collect(self()),
      ?MODULE:loop(collect_and_send(St));
    {subscribe,{Pid,Collectors}} ->
      ?MODULE:loop(subscr(St,Pid,Collectors));
    {unsubscribe,{Pid}} ->
      ?MODULE:loop(unsubscr(St,Pid));
    {'EXIT',Pid,_} ->
      ?MODULE:loop(unsubscr(St,Pid));
    {config,CollData} ->
      ?MODULE:loop(config(St,CollData));
    dbg ->
      F = fun(M,#collector{subscribers=S},A) -> [{mod,M},{subsc,S}|A] end,
      ?log([{pid,self()} | orddict:fold(F,[],St#st.collectors)]),
      ?MODULE:loop(St);
    stop ->
      ok
  end.

config(St,CollData) ->
  St#st{collectors=orddict:map(fun(K,V)->conf(K,V,CollData) end,
                               St#st.collectors)}.

conf(C,Collector,{C,Data}) ->
  State = C:config(Collector#collector.state,Data),
  Collector#collector{state=State};
conf(_,Collector,_) ->
  Collector.

subscr(St,Pid,Cs) ->
  link(Pid),
  {Pid,Collectors} = lists:foldl(fun subs/2,{Pid,St#st.collectors},Cs),
  St#st{collectors=Collectors}.

subs(C,{Pid,Collectors}) ->
  {Pid,orddict:store(C,collector(Collectors,Pid,C),Collectors)}.

collector(Colls,Pid,C) ->
  case orddict:find(C,Colls) of
    {ok,Coll = #collector{subscribers=Subs}} ->
      Coll#collector{subscribers=lists:umerge(Subs,[Pid])};
    error ->
      {State,_Data} = C:collect(init),
      #collector{subscribers=[Pid],
                 state=State}
  end.

unsubscr(St = #st{collectors = Colls},Pid) ->
  St#st{collectors=orddict:map(fun(_K,V)->unsubs(Pid,V) end,Colls)}.

unsubs(Pid,C) ->
  C#collector{subscribers=C#collector.subscribers--[Pid]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collect_and_send(St) ->
  St#st{collectors=cns(St#st.collectors)}.

cns(Colls) ->
  {NewColls,Datas} = orddict:fold(fun cns/3,{Colls,orddict:new()},Colls),
  send(Datas),
  NewColls.

cns(Module,Coll,{Colls,Datas}) ->
  case Coll#collector.subscribers of
    [] ->
      {Colls,Datas};
    Subs ->
      {NState,Data} = Module:collect(Coll#collector.state),
      {orddict:store(Module,Coll#collector{state=NState},Colls),
       stall(Data,Subs,Datas)}
  end.

stall(Data,Subs,Datas) ->
  lists:foldl(fun(S,Dict)->stll(S,Dict,Data) end,Datas,Subs).

stll(Sub,Dict,Data) ->
  case orddict:is_key(Sub,Dict) of
    true -> orddict:append(Sub,Data,Dict);
    false-> orddict:store(Sub,[Data],Dict)
  end.

send(Dict) -> orddict:fold(fun sendf/3,[],Dict).

sendf(Sub,Data,_) -> Sub ! {{data,node()},Data}.
