%%%-------------------------------------------------------------------
%%% File    : sherk_ets.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description :
%%%
%%% Created : 14 Feb 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(sherk_ets).

-export([assert/1]).
-export([new/1,new/2]).
-export([kill/1]).
-export([upd/2,upd/3]).
-export([lup/2]).
-export([f2t/1,t2f/2]). %file2tab, tab2file but compressed

-define(TAB(T), list_to_atom(atom_to_list(T)++"_tab")).

assert(Tab) ->
  case whereis(?TAB(Tab)) of
    undefined -> new(Tab);
    _ -> ok
  end.

new(Tab) -> new(Tab, [named_table,public,ordered_set]).
new(Tab, Opts) ->
  kill(Tab),
  Mama = self(),
  Ref = make_ref(),
  E = fun() -> register(?TAB(Tab),self()),
               ets:new(Tab,Opts),
               Mama ! Ref,
               receive {quit, P} -> P ! Tab end
      end,
  spawn(E),
  receive Ref -> ok end,
  Tab.

kill(Tab) ->
  try
    ?TAB(Tab) ! {quit, self()},
    receive Tab -> ok end
  catch _:_ -> ok
  end.

upd(Tab, Key) -> upd(Tab, Key, 1).
upd(Tab, Key, Inc) ->
  try ets:update_counter(Tab, Key, Inc)
  catch _:_ -> ets:insert(Tab, {Key, Inc}), Inc
  end.

lup(Tab, Key) ->
  try ets:lookup(Tab, Key) of
      [{Key,Val}] -> Val;
      O -> O
  catch _:_ -> []
  end.

%% tab2file/2 and file2tab/1 replacements
%% stores multiple tabs in compressed files

%% file format is;
%% <<TabInfoSize:32/integer>>,<<TabInfo:TabInfoSize/binary>>,
%% [<<ObjectSize:32/integer>>,<<Object:ObjectSize/binary>>,...]
%% <<0:32/integer>>

t2f(Tabs, File) when is_list(Tabs) ->
  case file:open(File,[write,compressed,raw,binary]) of
    {error,R} ->
      exit({error_opening,R,File});
    {ok,FD} ->
      try t2f(Tabs,FD,0)
      after file:close(FD)
      end
  end.

t2f([],_FD,N) -> N;
t2f([Tab|Tabs],FD,N) ->
  t2f_f(make_info(Tab),FD),
  ets:foldl(fun t2f_f/2,FD,Tab),
  file:write(FD,<<0:32/integer>>),
  t2f(Tabs,FD,N+1).

make_info(Tab) ->
  undefined==ets:info(Tab,size) andalso exit({no_such_table,Tab}),
  {ets:info(Tab,name),
   [ets:info(Tab,type),
    ets:info(Tab,protection),
    {keypos,ets:info(Tab,keypos)}]++
   case ets:info(Tab,named_table) of true -> [named_table];_ -> [] end}.

t2f_f(Term,FD) ->
  B = term_to_binary(Term),
  S = size(B),
  file:write(FD,<<S:32/integer,B/binary>>),
  FD.

f2t(File) ->
  case file:open(File,[read,compressed,raw,binary]) of
    {error,R} ->
      exit({error_opening,R,File});
    {ok,FD} ->
      try do_tabs(get_tab_header(FD),FD,[])
      after file:close(FD)
      end
  end.

do_tabs(eof,_FD,O) ->
  O;
do_tabs({Name,Opts},FD,O) ->
  new(Name,Opts),
  Cnt = f2t(FD,Name,0),
  do_tabs(get_tab_header(FD),FD,[{Name,Cnt}|O]).

get_tab_header(FD) ->
  try f2t_f(FD)
  catch
    throw:eof -> eof;
    _:X -> exit({bad_header,X})
  end.


f2t(FD,Tab,N) ->
  try ets:insert(Tab,f2t_f(FD)) of
    _ -> f2t(FD,Tab,N+1)
  catch
    throw:delimiter -> N;
    throw:eof -> exit({unexpected_eof})
  end.

f2t_f(FD) ->
  case file:read(FD,4) of
    eof -> throw(eof);
    {ok,<<0:32/integer>>} -> throw(delimiter);
    {ok,<<Size:32/integer>>} ->
      case file:read(FD,Size) of
        {ok,Bin} -> binary_to_term(Bin);
        R -> exit({error_reading_term,R})
      end;
    R -> exit({error_reading_size,R})
  end.
