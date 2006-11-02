%%%-------------------------------------------------------------------
%%% File    : prfPrc.erl
%%% Author  : Mats Cronqvist <qthmacr@duna283>
%%% Description : prf collector for per-process data
%%%
%%% Created :  8 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%%-------------------------------------------------------------------
-module(prfPrc).

-export([collect/1]).

-import(gb_trees,[empty/0,smallest/1,lookup/2,insert/3,delete/2,to_list/1]).

-record(cst,{info = empty()}).

-include("prf.hrl").

%%%reductions,message_queue_len,memory
%%%current_function,initial_call,registered_name

%%% returns {State, Data}
collect(init) -> 
    collect(#cst{});
collect(Cst) -> 
    {I,Red,Mem,Msg} = collect(processes(),Cst#cst.info,
			      empty(),empty(),empty(),empty()),
    {Cst#cst{info=I},{?MODULE,{post(I),post(Red,I),post(Mem,I),post(Msg,I)}}}.

collect([],_,NewInfo,Red,Mem,Msg) -> {NewInfo,Red,Mem,Msg};
collect([P|Ps],OldInfo,NewInfo,Red,Mem,Msg) ->
    case catch [pinf(P,T) || T <- [reductions,memory,message_queue_len]] of
	{'EXIT',_} ->
	    collect(Ps,OldInfo,NewInfo,Red,Mem,Msg);
	[Vred,Vmem,Vmsg] ->
	    {Reds,NNewInfo} = redder(P,Vred,Vmem,Vmsg,OldInfo,NewInfo),
	    NMem = tree_it(Mem,P,Vmem),
	    NMsg = tree_it(Msg,P,Vmsg),
	    NRed = tree_it(Red,P,Reds),
	    collect(Ps,OldInfo,NNewInfo,NRed,NMem,NMsg)
    end.

redder(P,Red,Mem,Msg,OldInfo,NewInfo) ->
    case lookup(P,OldInfo) of
	{value,{OldRed,_,_,_}} -> 
	    Dred = Red-OldRed,
	    {Dred,insert(P,{Red,Dred,Mem,Msg},NewInfo)};
	none -> 
	    {Red,insert(P,{Red,Red,Mem,Msg},NewInfo)}
    end.

tree_it(Tree,_Key,0) -> Tree;
tree_it(Tree,Key,Val) ->
    case gb_trees:size(Tree) of
	X when X < ?ITEMS -> insert({Val,Key},[],Tree);
	_ -> 
	    case smallest(Tree) of
		{K,_} when K < {Val,Key} -> insert({Val,Key},[],delete(K,Tree));
		_ -> Tree
	    end
    end.

post(Info) -> [{now,now()},{process_count,gb_trees:size(Info)}].

post(Tree,Info) -> 
    lists:foldl(fun({{_,P},[]},Acc)->postf(P,Acc,Info) end,[],to_list(Tree)).

-define(PINFS,[current_function,initial_call,registered_name]).
postf(P,Acc,Info) ->
    case catch pst(lookup(P,Info))++[{pid,P}|[{T,pinf(P,T)} || T <- ?PINFS]] of
	{'EXIT',_R} -> Acc;
	L -> [L|Acc]
    end.

pst({value,{Red,Dred,Mem,Msg}}) -> 
    [{reductions,Red},{diff_reds,Dred},{message_queue_len,Msg},{memory,Mem}].

pinf(Pid, Type = registered_name) ->
    case process_info(Pid, Type) of
	[] -> [];
	{Type,Val} -> Val
    end;
pinf(Pid, Type = initial_call) ->
    case process_info(Pid, Type) of
	{Type,{proc_lib,init_p,5}} -> proc_lib:translate_initial_call(Pid);
	{Type,{dets, do_open_file, 11}}->{dets, element(2, dets:pid2name(Pid))};
	{Type, Val} -> Val
    end;
pinf(Pid, Type) -> 
    {Type, Val} = process_info(Pid, Type),
    Val.
