%% -*- erlang-indent-level: 2 -*-
%%% Created : 13 Jan 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(watchdogLogger).
-author('Mats Cronqvist').

-export([init/1, terminate/1, tick/2, collectors/0,config/2]).
-record(cld, {node, fd}).

-include("log.hrl").

%% example usage;
%% prf:start(wd_logger,kred@sarv,watchdogLogger,'watchdog@ruda-ii').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectors() -> watchdog.

init(Node) ->
  FN = os:getenv("HOME")++"/log-"++atom_to_list(Node)++".gz",
  case file:open(FN,[write,compressed,raw]) of
    {ok,FD} -> ?log({opened,FN}),#cld{node=Node, fd=FD};
    {error,R} -> exit({error_opening,FN,R})
  end.

terminate(_LD) -> ok.
config(LD,_Data) -> ?log({loopdata,LD}), LD.

tick(LD, In) ->
  Bin = term_to_binary(In),
  Size = byte_size(Bin),
  case file:write(LD#cld.fd,<<Size:32,Bin/binary>>) of
    ok -> LD;
    {error,R} -> exit({error_writing,R,Size})
  end.
