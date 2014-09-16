%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 21 Aug 2014 by  <masse@klarna.com>

%% @doc
%% @end

-module('atop').
-author('').
-export([help/0]).
-export([getallocdata/0]).
-export([aggregate/0,aggregate/1,aggregate/2]).
-export([calls/0]).
-export([allocs/0,allocs/1,allocs/2]).

help_text() ->
  [" atop - a tool to visualize info about allocators."
   ,""
   ," The fundamental function returns a list of allocator info;"
   ,"getallocdata/0 -> list({AllocatorTags,Count})"
   ,""
   ," where"
   ,"AllocatorTags     -> [Allocator,Instance,Group,Type,Info]"
   ,"Allocator         -> atom(AllocatorName++\"_alloc\")"
   ,"AllocatorName     -> temp| std| sl| ll| fix| ets| eheap| driver| binary"
   ,"Instance          -> integer()"
   ,"Group             -> CarrierGroup | CallGroup"
   ,"CarrierGroup      -> sbcs| mbcs (singleblockcarriers| multiblockcarriers)"
   ,"CallGroup         -> calls"
   ,"Type              -> CarrierType | CallType"
   ,"CarrierType       -> sys | mseg | pool | blocks"
   ,"CallType          -> AllocatorSpecific | sys | mseg"
   ,"AllocatorSpecific -> AllocatorName"
   ,"Info              -> CallInfo | CarrierInfo"
   ,"CallInfo          -> alloc | realloc | dealloc | free"
   ,"CarrierInfo       -> count | size"
   ,"Count             -> size in bytes | counts"
   ,""
   ," The data can be aggregated, i.e. collapsed over e.g. the Instance."
   ,"aggregate(TagsToKeep,FilterTags) -> list({Tags,Count})"
   ,""
   ," whare"
   ,"TagsToKeep -> a list of which AllocatorTags not to collapse"
   ,"              list(allocator|instance|group|type|info)"
   ,"FilterTags -> a list of required tags, applied before collapsing"
   ,"              if the tag is {atom()}, its absence is required"
   ,""
   ,"aggregate(TagsToKeep) -> aggregate(TagsToKeep,[])"
   ,"aggregate() -> aggregate([allocator,group,type,info],[])"
   ,""
   ," For example;"
   ,"atop:aggregate([group,type],[size,blocks])."
   ,"[{[sbcs,sys],0},"
   ," {[sbcs,mseg],770048},"
   ," {[sbcs,blocks],768073},"
   ," {[mbcs_pool,pool],0},"
   ," {[mbcs_pool,blocks],0},"
   ," {[mbcs,sys],4922080},"
   ," {[mbcs,mseg],16777216},"
   ," {[mbcs,blocks],13028707}]"
   ,""
   ," The main functions are calls/0 and allocs/2."
   ," They print sorted and formatted info from aggregate/2."
   ,""
   ,"calls/0 -> ok"
   ,"allocs(SortColumn,TagsToKeep) -> ok"
   ,"allocs(SortColumn) -> allocs(SortColumn,[allocator,group])"
   ,"allocs() -> allocs(allocd,[allocator,group])"
   ,""
   ,"where"
   ,"SortColumn -> what|allocd|used|frac"
   ,"TagsToKeep -> list(allocator|instance|group)"
   ,""].

help() ->
  lists:foreach(fun(S)->io:fwrite("~s~n",[S])end,help_text()),
  {indices(),columns()}.

getallocdata() ->
  [{[A,N,K]++gettag(K,W),getvalue(W)} ||
    A <- erlang:system_info(alloc_util_allocators),
    {instance,N,Info} <- erlang:system_info({allocator,A}),
    {K,V} <- Info,
    K=/=options,
    W <- V,
    gettag(K,W)=/=drop].

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

calls() ->
  io:fwrite("~-20w~11w~11w~11w~11w~n",[allocator,alloc,dealloc,free,realloc]),
  lists:foreach(
    fun(L)->io:fwrite("~-20w~11w~11w~11w~11w~n",L)end,
    lists:foldl(
      fun({[X,alloc]  ,A},             T)  -> [[X,A,0,0,0]|T];
         ({[X,dealloc],D},[[X,A,0,0,0]|T]) -> [[X,A,D,0,0]|T];
         ({[X,free]   ,F},[[X,A,D,0,0]|T]) -> [[X,A,D,F,0]|T];
         ({[X,realloc],R},[[X,A,D,F,0]|T]) -> [[X,A,D,F,R]|T]
      end,
      [],
      lists:sort(aggregate([allocator,info],[calls])))).

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
          aggregate(AggregateIndices++[type,info],[size]))))).

printh(AggInds,Cols) ->
  io:fwrite("~-33w~8w~8w~8w~n",[AggInds]++tl(Cols)).
print({K,T,U,F}) ->
  io:fwrite("~-33w~8.2f~8.2f~8.2f~n",[K,T,U,F]).

present({K,V},A) ->
  case {A,lists:reverse(K)} of
    {[{NK,V0}|A0],[size,blocks|NK]} -> [format(NK,V,V0)|A0];
    {[{NK,V0}|A0],[size,_|NK]}      -> [{NK,V+V0}|A0];
    {_,[size,_|NK]}                 -> [{NK,V}|A]
  end.

format(NK,V,V0) ->
  {lists:reverse(NK),V0/1024/1024,V/1024/1024,divide(V,V0)}.

divide(_V,0) -> 1.0;
divide(V,V0) -> V/V0.

filter(Tags,PL) ->
  require_mandatory(Tags,remove_forbidden(Tags,PL)).

require_mandatory(Tags,PL) ->
  case [T || T <- Tags, is_atom(T)] of
    [] -> PL;
    Ms ->
      F = fun({K,_}) -> lists:member(true,[lists:member(M,K) || M <- Ms]) end,
      lists:filter(F,PL)
  end.

remove_forbidden(Tags,PL) ->
  case [T || {T} <- Tags, is_atom(T)] of
    [] -> PL;
    Fs ->
      F = fun({K,_}) -> not lists:member(true,[lists:member(F,K) || F <- Fs])
          end,
      lists:filter(F,PL)
  end.

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

gettag(K,W) ->
  case string:tokens(atom_to_list(element(1,W)),"_") of
    ["blocks"]                    -> [blocks,count];
    ["carriers"]              -> case K of mbcs_pool->[pool,count];_->drop end;
    ["carriers","size"]       -> case K of mbcs_pool->[pool,size];_->drop end;
    [V,"alloc","carriers"]        -> [list_to_atom(V),count];
    [V,"alloc","carriers","size"] -> [list_to_atom(V),size];
    [V]                           -> [list_to_atom(V),''];
    [V1,V2]                       -> [list_to_atom(V) || V <- [V1,V2]];
    [V0|Vs]            -> [list_to_atom(V) || V <- [V0,string:join(Vs,"_")]]
  end.

getvalue({_Tag,Current,_HighWaterSinceLastCall,_HighWater}) -> Current;
getvalue({_Tag,GigaCount,Count}) -> Count+1000000000*GigaCount;
getvalue({_Tag,Val}) -> Val.
