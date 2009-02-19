%% -*- erlang-indent-level: 2 -*-
%%% Created : 17 Oct 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(gen_serv).
-author('Mats Cronqvist').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the API

-export([start/1
         , start/2
         , stop/1
         , print_state/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server boiler plate

-behaviour(gen_server).
-export([handle_call/3
         , handle_cast/2
         , handle_info/2
         , init/1
         , terminate/2
         , code_change/3]).

-record(ld,{mod,args,cld}).

init({Mod,Args}) ->
  {_,NLD} = applie(#ld{mod=Mod,args=Args},init,[Args]),
  {ok,NLD}.

terminate(Reason,LD) -> 
  applie(LD,terminate,[Reason,LD#ld.cld]).

code_change(_,LD,Info) -> 
  {_,NLD} = applie(LD,code_change,["",LD#ld.cld,Info]),
  {ok,NLD}.

handle_cast(Msg,LD) -> 
  {_,NLD} = applie(LD,handle_cast,[Msg,LD#ld.cld]),
  {noreply,NLD}.

handle_call(Msg,From,LD) -> 
  {Reply,NLD} = applie(LD,handle_call,[Msg,From,LD#ld.cld]),
  {reply,Reply,NLD}.

handle_info(Msg,LD) -> 
  {_,NLD} = applie(LD,handle_info,[Msg,LD#ld.cld]),
  {noreply,NLD}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the API

start(Mod) ->
  start(Mod,[]).

start(Mod,Args) ->
  gen_server:start_link({local, Mod}, Mod, {Mod,Args}, []).

stop(Mod) -> 
  try gen_server:call(Mod,stop) 
  catch exit:{noproc,_} -> ok
  end.

print_state(Mod) ->
  gen_server:call(Mod,print_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation

applie(LD,F,As) ->
  {Reply,CLD} = applier(LD#ld.mod,F,As),
  {Reply,LD#ld{cld=CLD}}.

applier(M,F,As) ->
  case F of
    print_state -> {ok,print_state(M,last(As))};
    handle_call -> safer(M,F,As,{ok,last(As)});
    _           -> {ok,safer(M,F,As,last(As))}
  end.

safer(M,F,As,Default) ->
  case erlang:function_exported(M,F,length(As)) of
    false-> Default;
    true -> apply(M,F,As) 
  end.

last(L) -> hd(lists:reverse(L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utilities

print_state(M,CLD) ->
  print_term(expand_recs(M,CLD)).

print_term(Term) -> 
  print_term(group_leader(),Term).

print_term(FD,Term) -> 
  case node(FD) == node() of
    true -> error_logger:info_report(Term);
    false-> io:fwrite(FD," ~p~n",[Term])
  end.

expand_recs(M,List) when is_list(List) -> [expand_recs(M,L)||L<-List];
expand_recs(M,Tup) when is_tuple(Tup) -> 
  case tuple_size(Tup) of
    L when L < 1 -> Tup;
    L ->
      Fields = safer(M,record_info,[element(1,Tup)],[]),
      case L == length(Fields)+1 of
	false-> list_to_tuple(expand_recs(M,tuple_to_list(Tup)));
	true -> expand_recs(M,lists:zip(Fields,tl(tuple_to_list(Tup))))
      end
  end;
expand_recs(_,Term) -> Term.
