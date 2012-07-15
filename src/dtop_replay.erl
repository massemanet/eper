%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 28 Jun 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('dtop_replay').
-author('Mats Cronqvist').
-export([
         replay/2,replay/3
        ]).


replay(TrcFile,Node) -> replay(TrcFile,Node,[]).

replay(File,OldNode,Opts) ->
  Node = ck_node(OldNode),
  replay_trc:go(File,mk_dtop_fun(),mk_dtop_init(Node,Opts),[{node,Node}|Opts]).

ck_node(Node) ->
  case string:tokens(atom_to_list(Node),"@") of
    [_,_] -> Node;
    _     -> exit({not_a_node,Node})
  end.

mk_dtop_fun() ->
  fun(done,{C,Opts,_})                     -> [{items,C}|maybe_tab(Opts)];
     ({watchdog,_,_,ticker,D},{C,Opts,LD}) -> {C+1,Opts,tick(LD,D)};
     (_,State)                             -> State
  end.

mk_dtop_init(Node,Opts) ->
  {0,Opts,conf(dtopConsumer:init(Node),Opts)}.

conf(CLD,Opts) ->
  dtopConsumer:config(maybe_file(CLD,Opts),{items,no_pad}).

maybe_file(CLD,Opts) ->
  case {proplists:get_value(file,Opts),
        proplists:get_value(table_owner,Opts)} of
    {undefined,undefined} -> CLD;
    {FN,undefined} when is_list(FN) ->
      {ok,FD} = file:open(FN,[write]),
      dtopConsumer:config(CLD,{fd,FD});
    {undefined,Owner} ->
      Tab = owner_to_tab(Owner),
      ets:new(Tab,[ordered_set,named_table,{heir,Owner,'here'}]),
      ets:insert(Tab,{lineno,1}),
      dtopConsumer:config(CLD,{fd,{Tab,lineno}})
  end.

maybe_tab(Opts) ->
  case proplists:get_value(table_owner,Opts) of
    undefined -> [];
    Owner ->
      Tab = owner_to_tab(Owner),
      ets:delete(Tab,lineno),
      [{table,Tab}]
  end.

owner_to_tab(Owner) ->
  list_to_atom(pid_to_list(Owner)).

tick(LD,Data) ->
  dtopConsumer:tick(LD,Data).
