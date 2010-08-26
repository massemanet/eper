%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 29 Jun 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('replay_trc').
-author('Mats Cronqvist').

-export([go/1,go/2,go/3,go/4]).

go(File) ->
  go(File,[{count,3}]).

go(File,Opts) ->
  go(File,fun(done,O)->lists:reverse(O);(X,O)->[X|O]end,[],Opts).  

go(File,Fun,Acc) ->
  go(File,Fun,Acc,[{count,3}]).

go(File,Fun,Acc,Opts) ->
  maybe_to_file(do(File,Fun,Acc,Opts),Opts).

maybe_to_file(X,Opts) ->
  case file:open(proplists:get_value(to_file,Opts,""),[write]) of
    {ok,FD} -> io:fwrite(FD,"~p~n",[X]);
    _       -> X
  end.

do(File,Fun,Acc,Opts) ->
  try bread:trc(File,mk_fun(Fun,Opts),{1,Acc})
  catch R -> R
  end.

%% {watchdog,kred@mero,{1277,765403,871058},sysMon,[{tag,long_gc},...]}
 mk_fun(Fun,Opts) ->
  Max = proplists:get_value(count,Opts,-1),
  Nod = proplists:get_value(node,Opts,all),
  {Beg,End} = times(Opts),
  fun(Item,{N,Acc}) when N =:= Max-> throw(Fun(done,Fun(Item,Acc)));
     (Item,{N,Acc}) when element(2,Item)=:=Nod; Nod=:=all -> 
      case hms_from_now(element(3,Item)) of
        T when End < T  -> throw(Fun(done,Fun(Item,Acc)));
        T when Beg =< T -> {N+1,Fun(Item,Acc)};
        _               -> {N+1,Acc}
      end;
     (_,{N,Acc}) -> {N,Acc}
  end.

times(Opts) ->
  case proplists:lookup(time,Opts) of
    none -> 
      B = proplists:get_value(start_time,Opts,"00:00:00"),
      E = proplists:get_value(stop_time,Opts,"23:59:59")
  end,
  {hms_from_string(B),hms_from_string(E)}.

hms_from_now(Now) ->
  element(2,calendar:now_to_local_time(Now)).

hms_from_string(Str) ->
  case [list_to_integer(S) || S <- string:tokens(Str,":")] of
    [H]     -> {H,0,0};
    [H,M]   -> {H,M,0};
    [H,M,S] -> {H,M,S}
  end.

