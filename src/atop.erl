%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 21 Aug 2014 by  <masse@klarna.com>

%% @doc
%% {[Allocator,Instance,Group,Type,Info],Count}
%%
%% Allocator         -> atom(AllocatorName++"_alloc")
%% AllocatorName     -> temp| std| sl| ll| fix| ets| eheap| driver| binary
%% Instance          -> integer()
%% Group             -> CarrierGroup | CallGroup
%% CarrierGroup      -> sbcs | mbcs (multiblockcarriers | singleblockcarriers)
%% CallGroup         -> calls
%% Type              -> CarrierType | CallType
%% CarrierType       -> sys | mseg | blocks
%% CallType          -> AllocatorSpecific | sys | mseg
%% AllocatorSpecific -> AllocatorName
%% Info              -> CallInfo | CarrierInfo
%% CallInfo          -> alloc | realloc | dealloc | free
%% CarrierInfo       -> count | size
%% Count             -> size in bytes | counts
%% @end

-module('atop').
-author('').
-export([allocs/0,allocs/1,allocs/2]).
-export([aggregate/0,aggregate/1,aggregate/2]).

aggregate() ->
  aggregate(indices()--[instance]).
aggregate(AggregateIndices) ->
  aggregate(AggregateIndices,[]).
aggregate(AggregateIndices,FilterTags) ->
  aggr(
    AggregateIndices,
    filter(
      FilterTags,
      getallocdata())).

allocs() ->
  allocs(allocd).
allocs(SortCol) ->
  allocs(SortCol,[allocator,group]).
allocs(SortCol,AggregateIndices) ->
  printh(AggregateIndices,columns()),
  lists:foreach(
    fun print/1,
    lists:reverse(
      lists:keysort(
        column2index(SortCol),
        lists:foldl(
          fun present/2,
          [],
          filter(
            [size],
            aggr(
              AggregateIndices++[type,info],
              getallocdata())))))).

printh(AggInds,Cols) ->
  io:fwrite("~-33w~8w~8w~8w~n",[AggInds]++tl(Cols)).
print({K,T,U,F}) ->
  io:fwrite("~-33w~8.2f~8.2f~8.2f~n",[K,T,U,F]).

present([],A) -> A;
present({K,V},A) ->
  case lists:reverse(K) of
    [size,sys|NK]    -> [{NK,V}|A];
    [size,mseg|NK]   -> [{NK,V0}|A0] = A,
                        [{NK,V+V0}|A0];
    [size,blocks|NK] -> [{NK,V0}|A0] = A,
                          [{lists:reverse(NK),
                            V0/1024/1024,
                            V/1024/1024,
                            divide(V, V0)}|A0];
    _                -> A
  end.

divide(_V,0) -> 1.0;
divide(V,V0) -> V/V0.

filter([],PL) -> PL;
filter(Tags,PL) ->
  lists:filter(
    fun({K,_})->
        lists:member(true,[lists:member(T,K) || T <- Tags])
    end,
    PL).

aggr(IndexTags,List) ->
  Inds = [tag2index(T) || T <- IndexTags],
  lists:foldl(mkaggfun(Inds),[],lists:sort(mksortfun(Inds),List)).

columns() -> [what,allocd,used,frac].
indices() -> [allocator,instance,group,type,info].

column2index(X) -> x2index(X,columns()).
tag2index(X) -> x2index(X,indices()).

x2index(X,Xs) -> x2index(X,Xs,1).
x2index(X,[X|_],N) -> N;
x2index(X,[_|Xs],N) -> x2index(X,Xs,N+1).

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
             {Fst,Sec} when Fst<Sec -> throw(true);
             {Fst,Sec} when Sec<Fst -> throw(false);
             _                      -> same
           end || I <- Ind],
           throw(true)
      catch
        throw:R -> R
      end
  end.

getallocdata() ->
  [{[A,N,K]++gettag(W),getvalue(W)} ||
    A <- erlang:system_info(alloc_util_allocators),
    {instance,N,Info} <- erlang:system_info({allocator,A}),
    {K,V} <- Info,
    K=:=mbcs orelse K=:=sbcs orelse K=:=calls,
    W <- V,
    gettag(W) =/= drop].

gettag(W) ->
  case string:tokens(atom_to_list(element(1,W)),"_") of
    ["blocks"]                    -> [blocks,count];
    ["carriers"]                  -> drop;
    ["carriers","size"]           -> drop;
    [V,"alloc","carriers"]        -> [list_to_atom(V),count];
    [V,"alloc","carriers","size"] -> [list_to_atom(V),size];
    Vs                            -> [list_to_atom(V) || V <- Vs]
  end.

getvalue({_Tag,Current,_HighWaterSinceLastCall,_HighWater}) -> Current;
getvalue({_Tag,GigaCount,Count}) -> Count+1000000000*GigaCount;
getvalue({_Tag,Val}) -> Val.
