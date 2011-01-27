%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : prfNet.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : prf collector of net_kernel info
%%%
%%% Created : 18 Oct 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(prfNet).

-export([collect/1,config/2]).
-export([port_info/1]).

-include_lib("kernel/include/inet.hrl").

%%% returns {State, Data}
collect(State) -> {State, {?MODULE, data()}}.

config(State,_ConfigData) -> State.

data() -> lists:sort([port_info(P) || P <- erlang:ports()]).

%%returns {Name::atom(),Stats::list()}
port_info(P) when is_port(P) ->
  try stats(P,name(P))
  catch _:_ -> {P,[]}
  end.

stats(P,{driver,Name}) ->
  {{driver,Name},erlang:port_info(P)};
stats(P,Name) ->
  case erlang:port_info(P) of
    undefined -> throw(port_gone);
    Info ->
      try {ok,Stats} = inet:getstat(P), {Name,Info++Stats}
      catch _:_ -> {Name,Info}
      end
  end.

name(P) -> name(erlang:port_info(P,name),P).

name({name,"udp_inet"},P) ->
  {ok,Port} = inet:port(P),
  {udp,Port};
name({name,"tcp_inet"},P) ->
  {ok,{IP,Port}} = inet:peername(P),
  memoize({prfNet,tcp_name,IP,Port}, fun tcp_name/1);
name({name,Name},_P) ->
  {driver,Name}.

tcp_name({prfNet,tcp_name,IP,Port}) ->
  case inet:gethostbyaddr(IP) of
    {ok,#hostent{h_name=HostName}} ->
      try
        {ok,Names} = net_adm:names(HostName),
        {value,{NodeName,Port}} = lists:keysearch(Port,2,Names),
        {node,NodeName++"@"++HostName}
      catch
        _:_ -> {tcp,{HostName,Port}}
      end;
    _ ->
      X = tuple_to_list(IP),
      {tcp,{tl(lists:flatten([[$.,integer_to_list(I)]||I<-X])),Port}}
  end.

memoize(Key,F) ->
  case get(Key) of
    undefined -> put(Key,F(Key)), get(Key);
    Name -> Name
  end.
