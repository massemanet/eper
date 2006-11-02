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
-record(ld, {node,gtkPid,data=#data{},
	     mem_max=512*1024*1024,net_max=4000,load_max=1}).

collectors() -> [prfSys,prfNet].

init(Node) -> 
    {_,Reg} = process_info(self(),registered_name),
    GtkPid = gperfGtk:start(list_to_atom((atom_to_list(Reg)++"_GUI")),Node),
    link(GtkPid),
    #ld{node = Node, gtkPid=GtkPid}.

terminate(LD) -> gperfGtk:stop(LD#ld.gtkPid).

tick(LD, Data) -> 
    NLD = update_ld(LD, Data),
    NLD#ld.gtkPid ! {tick, {ltime(NLD#ld.data),info(NLD,LD)}},
    NLD.

config(LD,{maxs,[Mem,Net]}) ->
    LD#ld{net_max=Net,mem_max=Mem};
config(LD,dbg) ->
    dump_ld(LD),
    LD.

info(NLD,LD) ->
    [load(LD#ld.load_max,NLD#ld.data,LD#ld.data),
     mem(LD#ld.mem_max,NLD#ld.data,LD#ld.data),
     net(LD#ld.net_max,NLD#ld.data,LD#ld.data)].

dump_ld(LD) ->
    F = fun({N,V}) -> io:fwrite("~p~n",[{N,V}]) end,
    lists:foreach(F,zip(record_info(fields,ld),tl(tuple_to_list(LD)))).

zip([],[]) -> [];
zip([A|As],[B|Bs]) -> [{A,B}|zip(As,Bs)].

update_ld(LD,[]) -> 
    LD#ld{data=#data{}};
update_ld(LD,[[{prfNet,Net},{prfSys,Sys}]|_]) -> 
    LD#ld{data=#data{net=Net,sys=Sys}}.

ltime(#data{sys=Sys}) ->
    case catch calendar:now_to_local_time(w(now,Sys)) of
	{'EXIT',_} -> io:fwrite("bad time: ~p~n",[catch w(now,Sys)]),no_time;
	{_Date,Time}-> Time
    end.

load(Max,#data{sys=N},#data{sys=O}) ->
    User = w(user,N)/100,
    Kern = w(kernel,N)/100,
    Wait = w(wait,N)/100,
    case User of
	0.0 -> Beam = beamload(1,N,O);
	_ -> Beam = beamload(User,N,O)
    end,
    [Beam,User,User+Kern,max(User+Kern+Wait, Max)].

beamload(Max,N,O) ->
    case dv(w(runtime,N)-w(runtime,O),w(wall_clock,N)-w(wall_clock,O)) of
	Load when Load < 0 -> 0;
	Load when Max < Load -> Max;
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
net2([{in,I}|X],_,O) -> net2(X,I,O);
net2([{out,O}|X],I,_) -> net2(X,I,O);
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
