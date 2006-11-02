%%%-------------------------------------------------------------------
%%% File    : sherk_prof.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created :  2 Dec 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(sherk_prof).

-author('etxmacr@avc386').

-export([go/3]).

-import(lists,[member/2,reverse/1,dropwhile/2,prefix/2,splitwith/2]).

-define(LOG(T), sherk:log(process_info(self()),T)).

-record(state, {tab, currf, in = no, gc = no, fd = no, error}).

go(Msg, Line, initial) ->
    go(Msg, Line, #state{tab = sherk_ets:new(?MODULE)});
go(end_of_trace, _Line, State) ->
    State;
go(Msg, Line, State) ->
    do(Msg, Line, State).

do(Msg, Line, State) ->
    case catch handle(Msg, State) of
	NState = #state{error = undefined} -> NState;
	NState = #state{error = Err} ->
	    io:fwrite("~p: error on: ~p~n~p~n~p~n", [?MODULE, Line, Msg, Err]),
	    NState#state{error = undefined};
	{'EXIT', R} ->
	    exit({crashed, {?MODULE, Line, R, Msg}})
    end.

handle(Msg, State) ->
    case Msg of
	{out, _, 0, Now} ->			%>late r7 filedriver
	    leave_currf(Now, State#state{fd = yes});
	{in, _, 0, Now} ->			%>late r7 filedriver
	    enter_currf(Now, State#state{fd = no});
	{gc_start, {_Pid, _}, _Info, Now} ->	%gc
	    leave_currf(Now, State#state{gc = yes});
	{gc_end, {_Pid, _}, _Info, Now} ->	%gc
	    enter_currf(Now, State#state{gc = no});
	{in, {Pid, _}, CurrF, Now} ->
	    do_stack(in, State, Pid, CurrF),
	    enter_currf(Now, State#state{in = {Pid, Now}});
	{out, {_Pid, _}, _CurrF, Now} ->
	    (leave_currf(Now, State))#state{in = no};
	{spawn, _, {Pid, MFA}, _Now} ->
	    do_stack(in, State, Pid, check_mfa(MFA));
	{exit,{Pid, _}, _Reason, Now} -> %2 cases; killed pid is in or out
	    NState = leave_currf(Now, State),
	    do_stack(exit, NState, Pid, []),
	    case NState#state.in of
		{Pid, _} -> NState#state{in = no};
		_ -> NState
	    end;
	{call, {Pid, _}, MFA, Now} ->
	    NState = leave_currf(Now, State),
	    do_stack(call, NState, Pid, check_mfa(MFA)),
	    enter_currf(Now, NState);
	{return_to, {Pid, _}, MFA, Now} ->
	    NState = leave_currf(Now, State),
	    do_stack(return, NState, Pid, check_mfa(MFA)),
	    enter_currf(Now, NState);
	_ -> 
	    State
    end.


do_stack(in, State = #state{tab = Tab}, Pid, MFA) -> 
    case ets_lup(Tab, {stack, Pid}) of
	undefined -> ets_ins(Tab, {{stack, Pid}, [MFA]});
	_ -> ok
    end,
    State;
do_stack(exit, State = #state{tab = Tab}, Pid, _) ->
    ets_del(Tab, {stack, Pid}),
    State;
do_stack(call, State = #state{tab = Tab}, Pid, MFA) ->
    case ets_lup(Tab, {stack, Pid}) of
	undefined ->
	    do_stack(in, State, Pid, MFA);
	[MFA|_] ->				%tail recursion
	    ok;
	Stack -> 
	    %%case member(MFA, Stack) of
	    %%false ->
	    NewStack = [MFA|Stack],
	    ets_upd(Tab, {{func, calls}, MFA}),
	    ets_upd(Tab, {{func, calls}, Pid, MFA}),
	    ets_upd(Tab, {{stack, calls}, Pid, truncate(NewStack)}),
	    ets_ins(Tab, {{stack, Pid}, NewStack})%;
	%%true ->
	%%NS = maybe_trunc_stack(Stack, MFA),
	%%ets_ins(Tab, {{stack, Pid}, NS})
	%%end
    end,
    State;
do_stack(return, State = #state{tab = Tab}, Pid, MFA) ->
    case ets_lup(Tab, {stack, Pid}) of
	undefined ->
	    do_stack(in, State, Pid, MFA);
	Stack ->
	    case member(MFA, Stack) of
		false ->
		    ?LOG({dropped_headless_stack,{Pid,MFA,Stack}}),
		    drop_bad_stack(Tab, Pid, [MFA]),
		    do_stack(in, State, Pid, MFA);
		true ->
		    NS = dropwhile(fun(Mfa) -> MFA =/= Mfa end, Stack),
		    ets_ins(Tab, {{stack, Pid}, NS})
	    end
    end,
    State.

drop_bad_stack(Tab, Pid, Stack) ->
    ets_ins(Tab, {{stack, Pid}, Stack}),	% (more) correct stack
    ets_mdl(Tab, {{{stack, calls}, Pid, '_'}, '_'}), %all older stacks are bad
    ets_mdl(Tab, {{{stack, time}, Pid, '_'}, '_'}). %all older stacks are bad


check_mfa({M, F, As}) when list(As) -> {M, F, length(As)};
check_mfa(MFA) -> MFA.

enter_currf(Now, State = #state{in=In, gc=Gc, fd=Fd, currf=Currf}) ->
    case {In, Gc, Fd} of
	{{Pid, _}, no, no} -> 
	    case Currf of
		undefined -> State#state{currf = {Pid, Now}};
		_ -> State#state{error = {State}}
	    end;
	_ -> State
    end.

leave_currf(Now, State = #state{tab = Tab, in = In, gc = GC, currf = Currf}) ->
    case In of
	no -> 
	    State;
	_ -> 
	    case Currf of
		{Pid, NowIn} -> 
		    Tim = ntdiff(Now, NowIn),
		    Stack = ets_lup(Tab, {stack, Pid}),
		    Func = hd(Stack),
		    ets_upd(Tab, {total,time}, Tim),
		    ets_upd(Tab, {{pid,time}, Pid}, Tim),
		    ets_upd(Tab, {{func,time}, Func}, Tim),
		    ets_upd(Tab, {{func,time}, Pid, Func}, Tim),
		    ets_upd(Tab, {{stack,time}, Pid, truncate(Stack)}, Tim),
		    State#state{currf = undefined};
		undefined when GC == yes -> State;
		_ -> State#state{error = State}
	    end
    end.

truncate([]) -> [];
truncate([MFA|Stack]) ->
    case member(MFA,Stack) of
	false -> [MFA|truncate(Stack)];
	true ->
	    {Top,Bot} = splitwith(fun(Mfa) -> MFA/=Mfa end, Stack),
	    [MFA|Top]++truncate(remove_prefix([MFA|Top],Bot))
    end.

remove_prefix(Pref,List) ->
    case prefix(Pref,List) of
	true -> remove_prefix(Pref,List--Pref);
	false -> List
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_upd(Tab,Key) -> ets_upd(Tab,Key, 1).
ets_upd(Tab,Key,Inc) ->
    sherk_ets:upd(Tab,Key,Inc).
ets_ins(Tab, Rec) ->
    catch ets:insert(Tab, Rec).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	[{Key, R}] -> R;
	_ -> undefined
    end.
ets_del(Tab, Key) ->
    catch ets:delete(Tab, Key).
ets_mdl(Tab, Patt) ->
    catch ets:match_delete(Tab, Patt).

ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
