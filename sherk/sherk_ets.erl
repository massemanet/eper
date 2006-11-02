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
    case catch (list_to_atom(atom_to_list(Tab)++"_tab") ! {quit, self()}) of
	{'EXIT', _} -> ok;
	{quit,_} -> receive Tab -> ok end
    end.

upd(Tab, Key) -> upd(Tab, Key, 1).
upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } -> ets:insert(Tab, {Key, Inc}), Inc;
        O -> O
    end.

lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
        [{Key,Val}] -> Val;
	{'EXIT',_} -> [];
        O -> O
    end.

%% tab2file/2 and file2tab/1 replacements
%% stores multiple tabs in compressed files

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
    file:write(FD,<<0:32/integer>>),
    t2f_f(make_info(Tab),FD),
    ets:foldl(fun t2f_f/2,FD,Tab),
    t2f(Tabs,FD,N+1).

make_info(Tab) ->
    [exit({no_such_table,Tab}) || undefined==ets:info(Tab)], %woohoo
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
            try 
                catch f2t_f(FD),
                do_tabs(f2t_f(FD),FD,[])
            after 
                file:close(FD)
            end
    end.

do_tabs(eof,_FD,O) -> 
    O;
do_tabs({Name,Opts},FD,O) ->
    new(Name,Opts),
    Cnt = f2t(FD,Name,0),
    do_tabs(catch f2t_f(FD),FD,[{Name,Cnt}|O]).

f2t(FD,Tab,N) ->
    try 
        ets:insert(Tab,f2t_f(FD)),
        f2t(FD,Tab,N+1)
    catch
        throw:eof -> N;
        throw:bot -> N
    end.

f2t_f(FD) ->
    case file:read(FD,4) of
        {ok,<<0:32/integer>>} -> throw(bot);
	{ok,<<Size:32/integer>>} -> 
	    case file:read(FD,Size) of
		{ok,Bin} -> binary_to_term(Bin);
		{error,R} -> exit({error_reading,R});
		eof -> exit({unexpected_eof})
	    end;
	eof -> throw(eof);
	{error,R} -> exit({error_reading,R})
    end.
