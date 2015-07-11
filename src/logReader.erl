%% -*- erlang-indent-level: 2 -*-
%%% Created : 14 Jan 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('logReader').
-author('Mats Cronqvist').
-export([read/1]).

-dialyzer({no_return, read/2}).

read(FN) ->
  {ok,FD} = file:open(FN,[read,binary,compressed,raw]),
  try read(FD,<<>>)
  catch Thrown -> Thrown
  after file:close(FD)
  end.

read(FD,Cont) ->
  case file:read(FD,1048576) of
    {ok,Bin}  -> read(FD,chop(<<Cont/binary,Bin/binary>>));
    eof       -> throw(done);
    {error,R} -> exit({error_reading,R})
  end.

chop(<<Size:32,Bin:Size/binary,Size2:32,Bin2:Size2/binary,_/binary>>) ->
  throw({ok,binary_to_term(Bin),binary_to_term(Bin2)}).
