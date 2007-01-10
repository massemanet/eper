%%%-------------------------------------------------------------------
%%% File    : gperfConsumer.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 17 May 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(gperfConsumer).

-export([init/1, terminate/1, tick/2, collectors/0, config/2]).

-record(data,{net=[],sys=[]}).
-record(ld, {node,data=#data{},mem_max=0,net_max=0,load_max=0}).

-define(LOG(T), prf:log(process_info(self()),T)).
%%-define(LOG(T), ok).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collectors() -> [prfSys,prfNet].

init(Node) -> #ld{node = Node}.

terminate(_LD) -> ok.

tick(LD, Data) -> 
    NLD = update_ld(LD, Data),
    gperf ! {tick, {ltime(NLD#ld.data),info(NLD,LD)}},
    NLD.

config(LD,{mem,Mem}) ->
    %% gui unit is MByte
    LD#ld{mem_max=Mem*1024*1024};
config(LD,{net,Net}) ->
    %% gui unit is kByte/s
    LD#ld{net_max=Net*1024};
config(LD,{cpu,Cpu}) ->
    %% gui unit is %
    LD#ld{load_max=Cpu/100.0};
config(LD,dbg) ->
    dump_ld(LD),
    LD.

info(NLD,LD) ->
    [load(LD#ld.load_max,NLD#ld.data,LD#ld.data),
     mem(LD#ld.mem_max,NLD#ld.data,LD#ld.data),
     net(LD#ld.net_max,NLD#ld.data,LD#ld.data)].

dump_ld(LD) ->
    ?LOG(zip(record_info(fields,ld),tl(tuple_to_list(LD)))).

zip([],[]) -> [];
zip([A|As],[B|Bs]) -> [{A,B}|zip(As,Bs)].

update_ld(LD,[]) -> 
    LD#ld{data=#data{}};
update_ld(LD,[{prfNet,Net},{prfSys,Sys}]) -> 
    LD#ld{data=#data{net=Net,sys=Sys}};
update_ld(LD,[X]) -> 
    ?LOG({unrecognized_data,X}), LD.

ltime(#data{sys=Sys}) ->
    case T=w(now,Sys) of
        {_,_,_} -> {_,Time} = calendar:now_to_local_time(T),Time;
        Bad -> ?LOG({bad_time,Bad}),no_time
    end.

load(Max,#data{sys=N},#data{sys=O}) ->
    Beam = beamload(N,O),
    User = w(user,N)/100,
    Kern = w(kernel,N)/100,
    Wait = w(wait,N)/100,
    [max(1,dv(Beam,Max)),
     max(1,dv(User,Max)),
     max(1,dv(User+Kern,Max)),
     max(1,dv(User+Kern+Wait,Max))].

beamload(N,O) ->
    case dv(w(runtime,N)-w(runtime,O),w(wall_clock,N)-w(wall_clock,O)) of
	Load when Load < 0 -> 0;
	Load -> Load
    end.

mem(Max,#data{sys=Sys},_LD) ->
    [max(1,dv(w(beamsize,Sys),Max)),
     max(1,dv(w(total,Sys),Max)),
     max(1,dv(w(ets,Sys)+w(processes,Sys),Max)),
     max(1,dv(w(processes,Sys),Max))].

net(Max,#data{net=NetN},#data{net=NetO}) -> 
    {InO,OutO} = net1(NetO,0,0),
    {InN,OutN} = net1(NetN,0,0),
    [max(1,dv(InN-InO,Max)),
     max(1,dv(OutN-OutO,Max))].

net1([],In,Out) -> {In,Out};
net1([{_Nod,N}|X],I,O) -> 
    {In,Out} = net2(N,[],[]),
    net1(X,I+In,O+Out).

net2(_,I,O) when is_integer(I),is_integer(O) -> {I,O};
net2([{recv_oct,I}|X],_,O) -> net2(X,I,O);
net2([{send_oct,O}|X],I,_) -> net2(X,I,O);
net2([_|X],I,O) -> net2(X,I,O).

w(Tag,List) ->
    case lists:keysearch(Tag,1,List) of
	{value, {Tag,{Val,_}}} -> Val;
	{value, {Tag,Val}} -> Val;
	_ -> 0
    end.

dv(_,0) -> 0;
dv(A,B) -> A/B.

max(R,V) when V < R -> V;
max(R,_) -> R.
