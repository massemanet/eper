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
