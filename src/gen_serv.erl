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
         , print_state/1
         , unlink/0]).


%% the gen_server boilerplate
-behaviour(gen_server).
-export([handle_call/3
         , handle_cast/2
         , handle_info/2
         , init/1
         , terminate/2
         , code_change/3]).

-include("log.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the API

start(Mod) ->
  start(Mod,[]).

start(Mod,Args) ->
  gen_server:start_link({local, Mod}, ?MODULE, {Mod,Args}, []).

stop(Mod) -> 
  try gen_server:cast(Mod,stop) 
  catch exit:{noproc,_} -> ok
  end.

print_state(Mod) ->
  gen_server:call(Mod,print_state).

unlink() ->
  {links,Links} = process_info(self(),links),
  lists:foreach(fun unlink/1,Links).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server boiler plate

-record(ld,{mod,args,cld}).

init({Mod,Args}) ->
  safer(#ld{mod=Mod,args=Args},init,[Args]).

terminate(Reason,LD) -> 
  safer(LD,terminate,[Reason,LD#ld.cld]).

code_change(_,LD,Info) -> 
  safer(LD,code_change,["",LD#ld.cld,Info]).

handle_cast(Msg,LD) -> 
  safer(LD,handle_cast,[Msg,LD#ld.cld]).

handle_call(Msg,From,LD) -> 
  safer(LD,handle_call,[Msg,From,LD#ld.cld]).

handle_info(Msg,LD) -> 
  safer(LD,handle_info,[Msg,LD#ld.cld]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation

safer(LD,_,[stop|_]) ->
  {stop,shutdown,LD};
safer(LD,F,[print_state|As]) ->
  print_state(LD#ld.mod,last(As)),
  safe_default(F,LD);
safer(LD,F,As) ->
  case erlang:function_exported(LD#ld.mod,F,length(As)) of
    false-> safe_default(F,LD);
    true ->
      try safe_reply(F,LD,apply(LD#ld.mod,F,As)) 
      catch 
        error:R ->
          ?log([R,{mfa,{LD#ld.mod,F,As}},erlang:get_stacktrace()]),
          safe_default(F,LD);
        throw:R ->
          {stop,shutdown,R}
      end
  end.

safe_reply(handle_call,LD,{Reply,CLD}) -> {reply,Reply,LD#ld{cld=CLD}};
safe_reply(handle_cast,LD,CLD) -> {noreply,LD#ld{cld=CLD}};
safe_reply(handle_info,LD,CLD) -> {noreply,LD#ld{cld=CLD}};
safe_reply(_          ,LD,CLD) -> {ok,LD#ld{cld=CLD}}.

safe_default(handle_call,LD) -> {reply,ok,LD};
safe_default(handle_cast,LD) -> {noreply,LD};
safe_default(handle_info,LD) -> {noreply,LD};
safe_default(_          ,LD) -> {ok,LD}.

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

expand_recs(M,List) when is_list(List) ->
  [expand_recs(M,L)||L<-List];
expand_recs(M,Tup) when is_tuple(Tup) -> 
  case tuple_size(Tup) of
    L when L < 1 -> Tup;
    L ->
      {ok,#ld{cld=Fields}}=safer(#ld{mod=M,cld=[]},rec_info,[element(1,Tup)]),
      case L == length(Fields)+1 of
	false-> list_to_tuple(expand_recs(M,tuple_to_list(Tup)));
	true -> expand_recs(M,lists:zip(Fields,tl(tuple_to_list(Tup))))
      end
  end;
expand_recs(_,Term) ->
  Term.
