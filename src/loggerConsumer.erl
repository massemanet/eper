%% -*- erlang-indent-level: 2 -*-
%%% Created : 13 Jan 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('loggerConsumer').
-author('Mats Cronqvist').

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node, fd}).

-include("log.hrl").

%% example usage;
%% prf:start(logger,node(),loggerConsumer).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> [prfPrc,prfSys].

init(Node) -> 
  FN = os:getenv("HOME")++"log-"++atom_to_list(Node)++".gz",
  case file:open(FN,[write,compressed,raw]) of
    {ok,FD} -> ?log({opened,FN}),#cld{node=Node, fd=FD};
    {error,R} -> exit({error_opening,FN,R})
  end.

terminate(_LD) -> ok.
config(LD,_Data) -> ?log({loopdata,LD}), LD.

tick(LD, In) -> 
  case In of
    []	     -> Bin = <<"empty">>;
    [[]]     -> Bin = <<"empty_d">>;
    [Data|_] -> Bin = term_to_binary(Data)
  end,
  wrt(LD#cld.fd,Bin),
  LD.

wrt(FD,Bin) -> 
  Size = byte_size(Bin),
  case file:write(FD,<<Size:32,Bin/binary>>) of
    ok -> ok;
    {error,R} -> exit({error_writing,R,Size})
  end.
