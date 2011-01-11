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

replay(TrcFile,OldNode,Opts) ->
  Node = ck_node(OldNode),
  replay_trc:go(TrcFile,mk_dtop_fun(Node),mk_dtop_init(Node,Opts),Opts).

ck_node(Node) ->
  case string:tokens(atom_to_list(Node),"@") of
    [_,_] -> Node;
    _     -> exit({not_a_node,Node})
  end.

mk_dtop_fun(Node) ->
  fun(done,{C,_})                                   -> {items,C};
     ({watchdog,N,_,ticker,D},{C,LD}) when N=:=Node -> {C+1,tick(LD,D)};
     (_,{C,LD})                                     -> {C,LD}
  end.

mk_dtop_init(Node,Opts) ->
  {0,conf(dtopConsumer:init(Node),Opts)}.

conf(CLD,Opts) ->
  dtopConsumer:config(maybe_file(CLD,Opts),{items,no_pad}).

maybe_file(CLD,Opts) ->
  case file:open(proplists:get_value(file,Opts,""),[write]) of
    {ok,FD} -> dtopConsumer:config(CLD,{fd,FD});
    _ -> CLD
  end.

tick(LD,Data) ->
  dtopConsumer:tick(LD,Data).
