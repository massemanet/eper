%%%-------------------------------------------------------------------
%%% File    : dtop.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Created :  5 Sep 2005
%%% Description : top-like client for prf
%%%-------------------------------------------------------------------
-module(dtop).
-author('Mats Cronqvist').

-export([start/0,start/1,start/2,stop/0,
         replay/2,replay/3,
         sort/1,file/1]).

start() -> start(node()).

start([Node,Proxy]) -> start(Node,Proxy);
start([Node]) -> start([Node,no_proxy]);
start(Node) -> start([Node]).

start(Node,Proxy) when is_atom(Node),is_atom(Proxy) ->
  prf:start(dtop,Node,dtopConsumer,Proxy).

stop() -> prf:stop(dtop).

sort(Sort) -> prf:config(dtop,consumer,{sort,Sort}).

file(Filename) ->
  try {ok,FD} = file:open(Filename,[write]),
      prf:config(dtop,consumer,{fd,FD}),
      prf:config(dtop,consumer,{items,no_pad})
  catch _:R -> {error,{R,Filename}}
  end.

replay(TrcFile,Node) -> replay(TrcFile,Node,[]).

replay(TrcFile,Node,Opts) -> 
  try bread:trc(TrcFile,mk_dtop_fun(Node,Opts),mk_dtop_init(Node,Opts))
  catch R -> R
  end.

mk_dtop_fun(Node,Opts) ->
  Max = proplists:get_value(count,Opts,-1),
  fun(_,{I,C,_}) when I=:=Max                         -> throw({max,C});
     ({watchdog,N,_,ticker,D},{I,C,LD}) when N=:=Node -> {I+1,C+1,tick(LD,D)};
     (_,{I,C,LD})                                     -> {I+1,C,LD}
  end.

mk_dtop_init(Node,Opts) ->
  {0,0,dest_file(dtopConsumer:init(Node),Opts)}.

dest_file(CLD,Opts) ->
  try {ok,FD} = file:open(proplists:get_value(file,Opts,""),[write]),
      dtopConsumer:config(dtopConsumer:config(CLD,{fd,FD}),{items,no_pad})
  catch _:_ -> CLD
  end.

tick(LD,Data) ->
  dtopConsumer:tick(LD,Data).
