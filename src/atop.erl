%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 21 Aug 2014 by  <masse@klarna.com>

%% @doc
%% @end

-module('atop').
-author('').
-export([allocs/0,allocs/1,allocs/2]).

allocs() ->
  allocs(2).
allocs(SortI) ->
  allocs(SortI,[1,3]).
allocs(SortIndex,AggregateIndices) ->
  lists:foreach(
    fun print/1,
    lists:reverse(
      lists:keysort(
        SortIndex,
        lists:foldl(
          fun present/2,
          [],
          aggregate(
            AggregateIndices++[4],
            getallocdata()))))).

print({K,T,U,F}) ->
  io:fwrite("~-33w~8.2f~8.2f~8.2f~n",[K,T,U,F]).

present([],A) -> A;
present({K,V},A) ->
  case lists:reverse(K) of
    [carriers_size|NK] -> [{NK,V}|A];
    [blocks_size|NK]   -> [{NK,V0}|A0] = A,
                          [{lists:reverse(NK),
                            V0/1024/1024,
                            V/1024/1024,
                            divide(V, V0)}|A0]
  end.

divide(_V,0) -> 100.0;
divide(V,V0) -> V/V0.

aggregate(Inds,List) ->
  lists:foldl(mkaggfun(Inds),[],lists:sort(mksortfun(Inds),List)).

mkaggfun(Inds) ->
  fun({K,V},[]) -> [{mknewkey(K,Inds),V}];
     ({K,V},A)  -> NKey = mknewkey(K,Inds),
                   case hd(A) of
                     {NKey,V0} -> [{NKey,V0+V}|tl(A)];
                     _         -> [{NKey,V}|A]
                   end
  end.

mknewkey(K,Inds) ->
  [lists:nth(I,K)||I<-Inds].

mksortfun(Ind) ->
  fun({A,_},{B,_}) ->
      try [case {lists:nth(I,A),lists:nth(I,B)} of
             {Fst,Sec} when Fst<Sec -> error(true);
             {Fst,Sec} when Sec<Fst -> error(false);
             _-> same
           end || I <- Ind],
           true
      catch
        error:R -> R
      end
  end.

getallocdata() ->
  [{[A,N,K,element(1,W)],element(2,W)} ||
    A <- erlang:system_info(alloc_util_allocators),
    {instance,N,Info} <- erlang:system_info({allocator,A}),
    {K,V} <- Info,
    K=/=calls andalso K=/=options andalso K=/=version,
    W <- V,
    element(1,W)=:=blocks_size orelse element(1,W)=:=carriers_size].
