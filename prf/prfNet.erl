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
-include_lib("kernel/include/inet.hrl").

%%% returns {State, Data}
collect(State) -> {State, {?MODULE, data()}}.

config(State,_ConfigData) -> State.

data() -> lists:sort(data(erlang:ports())).

data([]) -> [];
data([P|Ports]) ->
  try [stats(P,name(P))|data(Ports)] 
  catch _:_ -> data(Ports) 
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
  end;
name({name,Name},_P) ->
  {driver,Name}.
