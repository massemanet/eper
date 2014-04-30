%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 29 Jun 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% Opts - list({Tag,Val})
%% {Tag,Val}: {to_file,string(Filename)}
%%            {count,int()}
%%            {node,atom()}
%%            {time,hms_string()}
%%            {delta_time,hms_string()}
%%            {start_time,hms_string()}
%%            {stop_time,hms_string()}
%% hms_string() - "HH:MM:SS"
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
  try bread:trc(File,mk_fun(Fun,Opts),{0,Acc})
  catch R -> R
  end.

%% {watchdog,kred@mero,{1277,765403,871058},sysMon,[{tag,long_gc},...]}
mk_fun(Fun,Opts) ->
  Max = proplists:get_value(count,Opts,-1),
  Nod = proplists:get_value(node,Opts,all),
  {Beg,End} = times(Opts),
  fun(Item,{N,Acc}) when N =:= Max-> throw(Fun(done,Fun(Item,Acc)));
     (Item,{N,Acc}) when element(2,Item)=:=Nod; Nod=:=all ->
      try hms_from_now(element(3,Item)) of
        T when End < T  -> throw(Fun(done,Fun(Item,Acc)));
        T when Beg =< T -> {N+1,Fun(Item,Acc)};
        _               -> {N,Acc}
      catch
        error:_ -> {N+1,Fun(Item,Acc)}
      end;
     (_,{N,Acc}) -> {N,Acc}
  end.

times(Opts) ->
  case proplists:lookup(time,Opts) of
    {time,HMS} ->
      Range = hms_from_string(proplists:get_value(delta_time,Opts,"00:00:10")),
      Begin = hms_from_string(HMS),
      {Begin,hms_add(Begin,Range)};
    none ->
      {hms_from_string(proplists:get_value(start_time,Opts,"00:00:00")),
       hms_from_string(proplists:get_value(stop_time,Opts,"23:59:59"))}
  end.

hms_from_sec(Secs) ->
  try {{0,1,1},HMS} = calendar:gregorian_seconds_to_datetime(Secs), HMS
  catch _:_ -> {23,59,59}
  end.

sec_from_hms(HMS) ->
  calendar:datetime_to_gregorian_seconds({{0,1,1},HMS}).

hms_add(A,B) ->
  hms_from_sec(sec_from_hms(A)+sec_from_hms(B)).

hms_from_now(Now) ->
  element(2,calendar:now_to_local_time(Now)).

hms_from_string(Str) ->
  case [list_to_integer(S) || S <- string:tokens(Str,":")] of
    [H]     when H < 24 -> {H,0,0};
    [H,M]   when H < 24 -> {H,M,0};
    [H,M,S] when H < 24 -> {H,M,S}
  end.
