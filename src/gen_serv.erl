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
         , shutdown/1
         , print_state/1
         , get_state/1
         , get_state/2
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
  cast(Mod,stop).

shutdown(Mod) ->
  cast(Mod,shutdown).

get_state(Mod,Field) ->
  case proplists:is_defined(Field,State = get_state(Mod)) of
    true -> proplists:get_value(Field,State);
    false-> exit({no_such_field,Field,State})
  end.

get_state(Mod) ->
  case whereis(Mod) of
    undefined -> exit({not_started,Mod});
    _         -> gen_server:call(Mod,get_state)
  end.

print_state(Mod) ->
  print_term(get_state(Mod)).

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

code_change(OldVsn,LD,Info) ->
  safer(LD,code_change,[OldVsn,LD#ld.cld,Info]).

handle_cast(Msg,LD) ->
  safer(LD,handle_cast,[Msg,LD#ld.cld]).

handle_call(Msg,From,LD) ->
  safer(LD,handle_call,[Msg,From,LD#ld.cld]).

handle_info(Msg,LD) ->
  safer(LD,handle_info,[Msg,LD#ld.cld]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation

safer(LD,_,[stop|_]) ->
  {stop,normal,LD};
safer(LD,_,[shutdown|_]) ->
  {stop,shutdown,LD};
safer(LD,F,[get_state|As]) ->
  CLD = last(As),
  safe_reply(F,LD,{expand_recs(LD#ld.mod,CLD),CLD});
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

safe_reply(_,LD,{stop,R,CLD})          -> {stop,R,LD#ld{cld=CLD}};
safe_reply(handle_call,LD,{Reply,CLD}) -> {reply,Reply,LD#ld{cld=CLD}};
safe_reply(handle_call,LD,CLD)         -> {noreply,LD#ld{cld=CLD}};
safe_reply(handle_cast,LD,CLD)         -> {noreply,LD#ld{cld=CLD}};
safe_reply(handle_info,LD,CLD)         -> {noreply,LD#ld{cld=CLD}};
safe_reply(_          ,LD,CLD)         -> {ok,LD#ld{cld=CLD}}.

safe_default(init,LD)        -> {ok,LD#ld{cld={}}};
safe_default(handle_call,LD) -> {reply,ok,LD};
safe_default(handle_cast,LD) -> {noreply,LD};
safe_default(handle_info,LD) -> {noreply,LD};
safe_default(_          ,LD) -> {ok,LD}.

last(L) -> hd(lists:reverse(L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utilities

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
      try Fields = M:rec_info(element(1,Tup)),
          L = length(Fields)+1,
          lists:zip(Fields,expand_recs(M,tl(tuple_to_list(Tup))))
      catch _:_ ->
          list_to_tuple(expand_recs(M,tuple_to_list(Tup)))
      end
  end;
expand_recs(_,Term) ->
  Term.

cast(Mod,What) ->
  try gen_server:cast(Mod,What)
  catch exit:{noproc,_} -> ok
  end.
