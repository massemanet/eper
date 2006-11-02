x%%%-------------------------------------------------------------------
%%% File    : sherk_new_prof.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 30 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_new_prof).

-export([go/3]).
-import(lists,[dropwhile/2,member/2]).

-record(state, {start}).

-define(LOG(T), sherk:log(process_info(self()),T)).

%%go(Msg,Seq,initial) -> {ok,FD}=file:open("/export/localhome/locmacr/pan/msgs.txt",[write]),go(Msg,Seq,FD);
%%go(Msg,Seq,FD) -> io:fwrite(FD,"~.10w~p~n",[Seq,Msg]),FD;
%%go(end_of_trace, _Seq, FD) -> file:close(FD);
go(Msg, Seq, initial) 	     -> go(Msg, Seq, init());
go(end_of_trace, Seq, State) -> terminate(Seq,State), State;
go(Msg, Seq, State) 	     -> handler(Msg) ! {msg,Seq,Msg}, State.

init() -> 
    ?LOG([{starting,?MODULE}]),
    sherk_ets:new(?MODULE),
    sherk_ets:new(sherk_prof),
    #state{start=now()}.

terminate(Seq,#state{start=Start}) -> 
    ?LOG([{finishing,?MODULE},
	  {seq,Seq},
	  {time,timer:now_diff(now(),Start)/1000000},
	  {procs,ets:info(?MODULE,size)}]),
    TermFun = fun({{handler,_},Pid},_) -> Pid ! quit; (_,_) -> ok end,
    ets:foldl(TermFun,[],?MODULE).

handler({spawn,_,{Pid,_},_,_}) -> assert_handler(Pid);
handler({_,{Pid,_},_,_}) -> assert_handler(Pid).

assert_handler(Pid) ->
    case sherk_ets:lup(?MODULE,{handler,Pid}) of
	[] -> 
	    P = spawn_link(fun handler/0),
	    P ! {pid,Pid},
	    ets:insert(?MODULE,{{handler,Pid},P}),
	    P;
	HandlerPid -> 
	    HandlerPid
    end.

-record(hstate,{gc=no,fd=no,sched=no,currf,ts}).
-record(hld,{stack=[],hstate=#hstate{},pid}).

handler() -> 
    receive
	{pid,Pid} -> handler_loop(#hld{pid=Pid});
	quit -> ok
    end.

handler_loop(HLD) ->
    receive
	{msg,_Seq,{Tag,_,Info,TS}} -> handler_loop(hand(Tag,Info,TS,HLD));
	quit -> ok
    end.

hand(Tag,Info,TS,HLD = #hld{pid=Pid,stack=OldStack,hstate=OldState}) ->
    {Sig,Stack} = stack(Tag,Info,OldStack),
    State = state(Sig, Stack, TS, OldState),
    tab(Sig,Pid,State,OldState),
    HLD#hld{stack=Stack,hstate=State}.

stack(out,0,Stack) 		-> {in_fd,Stack};
stack(in,0,Stack) 		-> {out_fd,Stack};
stack(gc_start,_,Stack) 	-> {in_gc,Stack};
stack(gc_end,_,Stack) 		-> {out_gc,Stack};
stack(out,MFA,[]) 		-> {out_sched,[MFA]};
stack(in,MFA,[]) 		-> {in_sched,[MFA]};
stack(out,MFA,[MFA|Stack]) 	-> {out_sched,[MFA|Stack]};
stack(in,MFA,[MFA|Stack]) 	-> {in_sched,[MFA|Stack]};
stack(call,MFA,[MFA|Stack]) 	-> {[],[arity(MFA)|Stack]};
stack(call,MFA,Stack) 		-> {stack,[arity(MFA)|Stack]};
stack(return_to,MFA,Stack) 	-> {stack,return(arity(MFA),Stack)};
stack(spawn,{_,MFA},[]) 	-> {[],[arity(MFA)]};
stack(exit,_,Stack) 		-> {out_sched,Stack};
stack(getting_linked,_,Stack)	-> {[],Stack};
stack(getting_unlinked,_,Stack)	-> {[],Stack};
stack(unlink,_,Stack) 		-> {[],Stack};
stack(link,_,Stack) 		-> {[],Stack};
stack(_,_,Stack) 		-> {[],Stack}.

state(in_fd,_,TS,State)       -> State#hstate{fd=yes,ts=TS};
state(out_fd,_,TS,State)      -> State#hstate{fd=no,ts=TS};
state(in_gc,_,TS,State)       -> State#hstate{gc=yes,ts=TS};
state(out_gc,_,TS,State)      -> State#hstate{gc=no,ts=TS};
state(in_sched,_,TS,State)    -> State#hstate{sched=yes,ts=TS};
state(out_sched,_,TS,State)   -> State#hstate{sched=no,ts=TS};
state(stack,[MFA|_],TS,State) -> State#hstate{currf=MFA,ts=TS};
state([],_,_,State) 	      -> State.

%#hstate{currf=CF,ts=TS},#hstate{currf=OCF,ts=OTS}) ->

tab([],_,_,_) -> ok;

tab(stack,Pid,#hstate{currf=CF,ts=TS},#hstate{currf=OCF,ts=OTS}) ->
    upd({{func,calls},CF},1),
    upd({{func,calls},Pid,CF},1),
    try 
	T = timer:now_diff(TS,OTS),
	upd({total,time},T),
	upd({{pid,time},Pid},T),
	upd({{func,time},OCF},T),
	upd({{func,time},Pid,OCF},T)
    catch
	_:_ -> ok
    end;

tab(in_sched,_,_,_) -> ok;

tab(out_sched,Pid,#hstate{fd=no,gc=no,currf=CF,ts=TS},#hstate{ts=OTS}) ->
    try 
	T = timer:now_diff(TS,OTS),
	upd({total,time},T),
	upd({{pid,time},Pid},T),
	upd({{func,time},CF},T),
	upd({{func,time},Pid,CF},T)
    catch
	_:_ -> ok
    end;

tab(in_gc,Pid,#hstate{sched=yes,ts=TS,currf=CF},#hstate{ts=OTS}) ->
    upd({total,gc},1),
    upd({{pid,gc},Pid},1),
    try 
	T = timer:now_diff(TS,OTS),
	upd({total,time},T),
	upd({{pid,time},Pid},T),
	upd({{func,time},CF},T),
	upd({{func,time},Pid,CF},T)
    catch
	_:_ -> ok
    end;

tab(in_gc,Pid,#hstate{sched=no,ts=TS},#hstate{ts=OTS}) ->
    upd({total,gc},1),
    upd({{pid,gc},Pid},1),
    try 
	T = timer:now_diff(TS,OTS),
	upd({total,time},T),
	upd({{pid,time},Pid},T)
    catch
	_:_ -> ok
    end;

tab(out_gc,_Pid,_,_) -> ok;

tab(in_fd,_Pid,_,_) -> io:fwrite("in_fd",[]);
tab(out_fd,_Pid,_,_) -> io:fwrite("out_fd",[]);
tab(Sig,Pid,HS,OHS) -> 
    ?LOG([{sig,Sig},{pid,Pid},{state,HS},{old_state,OHS}]).
    

return(MFA,Stack) ->
    case member(MFA,Stack) of
	true -> dropwhile(fun(Mfa) -> MFA/=Mfa end, Stack);
	false -> [MFA]
    end.

arity({M,F,A}) when is_list(A) -> {M,F,length(A)};
arity(MFA) -> MFA.

upd(Key,Inc) ->
    sherk_ets:upd(sherk_prof,Key,Inc).
