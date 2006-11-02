%%%----------------------------------------------------------------------
%%% File    : shrerk_scan.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 27 Feb 2006 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(sherk_scan).
-author('etxmacr@avc386').

-include_lib("kernel/include/file.hrl").

-export([action/5]).

-import(lists,[member/2,reverse/1,keysearch/3,map/2]).

-define(LOG(T), sherk:log(process_info(self()),T)).

-record(state, {seq=0, hits=0, cbs, pattern, out, min, max, eof = false}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(FileName, Patt, CBs, Min, Max) ->
    sherk_ets:new(?MODULE),
    {ok, FD} = file:open(FileName, [read, raw, binary,compressed]),
    State = #state{pattern=Patt,cbs=cbs(CBs),min=Min,max=Max},
    St = file_action(FD, State),
    file:close(FD),
    {hits, St#state.hits}.

-define(CHUNKSIZE, 1024).
file_action(FD, Stat) ->
    file_action(make_chunk(FD, <<>>), FD, Stat).

file_action(_, _FD, #state{eof = true} = Stat) -> 
    do(end_of_trace, Stat);
file_action(eof, _FD, Stat) -> 
    do(end_of_trace, Stat);
file_action({Term, Rest}, FD, Stat) ->
    file_action(make_chunk(FD, Rest), FD, do(Term, Stat)).
make_chunk(_FD, eof) -> eof;
make_chunk(FD, <<_Op, Size:32, Tal/binary>> = Bin) ->
    case Tal of
	<<Term:Size/binary, Tail/binary>> -> {binary_to_term(Term), Tail};
	_ -> make_chunk(FD, get_more_bytes(FD, Bin))
    end;
make_chunk(FD, Bin) -> make_chunk(FD, get_more_bytes(FD, Bin)).
get_more_bytes(FD, Rest) ->
    case file:read(FD, ?CHUNKSIZE) of
	{ok, Bin} -> <<Rest/binary, Bin/binary>>;
	_ -> eof
    end.

%%% CBs - CB|list(CB)
%%% CB - fun(F)|atom(M)|{fun(F),term(Init)}|{atom(M),term(Init)}
cbs([]) -> [];
cbs([CB|T]) -> [to_cb(CB)|cbs(T)];
cbs(Term) -> cbs([Term]).

to_cb('') -> {fun write_msg/3,[]};
to_cb(Mod) when is_atom(Mod) -> to_cb({Mod,initial});
to_cb(Fun) when is_function(Fun) -> to_cb({Fun,initial});
to_cb({Mod,Init}) when is_atom(Mod) -> is_cb(Mod),{{Mod,go},Init};
to_cb({Fun,Init}) when is_function(Fun) -> is_cb(Fun),{Fun,Init}.

is_cb(M) when is_atom(M) -> true = member({go,3},M:module_info(exports));
is_cb(F) when is_function(F) -> {arity,3} = erlang:fun_info(F,arity).

do(end_of_trace, State) -> 
    do_do(end_of_trace, State);
do(_Mess, #state{max = Max, seq = Seq} = Stat) when Max < Seq -> 
    Stat#state{eof=true};
do(Mess, #state{min = Min, seq = Seq} = Stat) when Seq < Min -> 
    case mass(Mess) of
	[] -> Stat;
	_ -> Stat#state{seq = Seq+1}
    end;
do(Mess, Stat) ->
    case mass(Mess) of
	[] -> Stat;
	Ms -> do_do(Ms, Stat)
    end.

do_do(end_of_trace = Ms, #state{cbs=CBs, seq=Seq} = State) ->
    State#state{cbs=do_safe_cbs(CBs, Ms, Seq, [])};
do_do(Mess, #state{pattern=Patt, cbs=CBs, seq=Seq} = State) ->
    case grep(Patt, Mess) of
	false -> State#state{seq = Seq+1};
	true -> 
	    State#state{seq = Seq+1, 
			hits = State#state.hits+1, 
			cbs=do_safe_cbs(CBs, Mess, Seq, [])}
    end.

do_safe_cbs([], _, _, O) -> reverse(O);
do_safe_cbs([CB|CBs], Msg, Seq, O) ->
    do_safe_cbs(CBs, Msg, Seq, [safe_cb(CB,Msg,Seq)|O]).

safe_cb({{M,F},State},Msg,Seq) -> {{M,F},M:F(Msg,Seq,State)};
safe_cb({Fun,State},Msg,Seq) -> {Fun,Fun(Msg,Seq,State)}.

write_msg(Msg,Seq,_) ->
    io:fwrite("~.9.0w ~w~n",[Seq,Msg]).

grep('',_) -> true;
grep(P,T) when list(P) ->
    case grp(P, T) of
	[] -> true;
	_ -> false
    end;
grep(P, T) -> grep([P], T).

grp([], _) -> [];
grp(P, []) -> P;
grp(P, Fun) when function(Fun) -> grp(P, list_to_atom(erlang:fun_to_list(Fun)));
grp(P, Port) when port(Port) -> grp(P, list_to_atom(erlang:port_to_list(Port)));
grp(P, Rf) when reference(Rf) -> grp(P, list_to_atom(erlang:ref_to_list(Rf)));
grp(P, Pid) when pid(Pid) -> grp(P, list_to_atom(pid_to_list(Pid)));
grp(P, T) when tuple(T) -> 
    case lists:member(T,P) of
	true -> grp(P--[T], []);
	false -> grp(P,tuple_to_list(T))
    end;
grp(P, L) when list(L) -> 
    case lists:member(L, P) of
	true -> grp(P--[L], []);
	false -> grp(grp(P, hd(L)), tl(L))
    end;
grp(P, T) -> grp(P--[T], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% format is {Tag, PI, Data, Timestamp}
%%% PI is {pid(), Info} 
%%% Info is atom()|tuple()
%%% Timestamp is no_time|now()
%%% Data is;
%%%
%%% send,                         {PI2, Msg}
%%% send_to_non_existing_process, {PI2, Msg}
%%% 'receive',                    Message
%%% call,                         {M,F,A}
%%% return_to,                    {M,F,A}
%%% return_from,                  {{M,F,A}, ReturnValue}
%%% spawn,                        {PI2, {M,F,A}}
%%% exit,                         Reason
%%% link,                         PI2
%%% unlink,                       PI2
%%% getting_linked,               PI2
%%% getting_unlinked,             PI2
%%% register                      registered_name
%%% unregister                    registered_name
%%% in,                           {M,F,A}
%%% out,                          {M,F,A}
%%% gc_start,                     Info
%%% gc_end,                       Info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mass(end_of_trace = T) ->  T;

mass({port_info, Info}) -> handle_porti(Info), [];
mass({proc_info, Info}) -> handle_proci(Info), [];
mass({trace_info, Info}) -> handle_traci(Info), [];

mass({trace, A, B, C}) -> mass(A, B, C, no_time);
mass({trace, A, B, C, D}) -> mass(A, B, {C, D}, no_time);
mass({trace_ts, A, B, C, TS}) -> mass(A ,B, C, TS);
mass({trace_ts, A, B, C, D, TS}) -> mass(A, B, {C, D}, TS);
mass(X) -> ?LOG({unrec_msg, X}), [].

mass(Pid, T=send, X, TS) ->                       mass_send(Pid,T,X,TS);
mass(Pid, T=send_to_non_existing_process, X,TS) ->mass_send(Pid,T,X,TS);
mass(Pid, T='receive', Message, TS) ->              {T,pi(Pid),Message,TS};
mass(Pid, T=call, MFA, TS) ->                       {T,pi(Pid),MFA,TS};
mass(Pid, T=return_to, MFA, TS) ->                  {T,pi(Pid),MFA,TS};
mass(Pid, T=return_from, {MFA,R}, TS) ->            {T,pi(Pid),{MFA,R},TS};
mass(Pid, T=spawn, {P2, MFA}, TS) ->    hps(P2,MFA),{T,pi(Pid),{pi(P2),MFA},TS};
mass(Pid, T=exit, Reason, TS) ->                    {T,pi(Pid),Reason,TS};
mass(Pid, T=link, Pd, TS) ->                        {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=unlink, Pd, TS) when pid(Pd) ->         {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=unlink, _Crap, TS) ->                   {T,pi(Pid),unknown,TS};
mass(Pid, T=getting_linked, Pd, TS) ->              {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=getting_unlinked, Pd, TS) ->            {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=register, Name, TS) ->    hpr(Pid,Name),{T,pi(Pid),Name,TS};
mass(Pid, T=unregister, Name, TS) ->                {T,pi(Pid),Name,TS};
mass(Pid, T=in, MFA, TS) ->                         {T,pi(Pid),MFA,TS};
mass(Pid, T=out, MFA, TS) ->                        {T,pi(Pid),MFA,TS};
mass(Pid, T=gc_start, Info, TS) ->                  {T,pi(Pid),Info,TS};
mass(Pid, T=gc_end, Info, TS) ->                    {T,pi(Pid),Info,TS}.

mass_send(Pid, T, {Msg, To}, TS) when pid(To); port(To) -> 
    {T,pi(Pid),{pi(To), Msg},TS};
mass_send(Pid, T, {Msg, To}, TS) when atom(To) -> 
    {T,pi(Pid),{{find_pid(To),To}, Msg},TS};
mass_send(Pid, T, {Msg, {To,Node}}, TS) when atom(To), Node==node(Pid) -> 
    {T,pi(Pid),{{find_pid(To),To}, Msg},TS};
mass_send(Pid, T, {Msg, {To,Node}}, TS) when atom(To), atom(Node) -> 
    {T,pi(Pid),{{remote,{To,Node}}, Msg},TS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pi(file_driver) -> {trace,file_driver};
pi(Port) when is_port(Port) ->
    case ets_lup(Port) of
	[] -> {Port, unknown};
	Name -> {Port, Name}
    end;
pi(Pid) when is_pid(Pid) ->
    case ets_lup(Pid) of
	[] -> {Pid, unknown};
	Name -> {Pid, Name}
    end.

find_pid(Name) -> ets_lup(Name).


handle_porti(Is) -> lists:foreach(fun(I)->ets_ins(I) end, Is).

handle_proci(Is) -> lists:foreach(fun proci/1, Is).

handle_traci(_I) -> ok.
    
proci({Pid,Name}) ->
    ets_ins({Pid,Name}),
    case is_atom(Name) of
	true -> ets_ins({Name,Pid});
	false -> ok
    end.

hps(Pid,{M,F,As}) -> hpr(Pid,mangle_ic({M,F,As})).

hpr(Pid, Reg) ->
    ets_ins({Pid,Reg}),
    ets_ins({Reg,Pid}).

mangle_ic(MFA) ->
    case MFA of
	{proc_lib,init_p,[_,_,M,F,A]} -> 
	    {proc_lib, trans_init(M,F,A)};
	{file,file,[_,FileName,_]} ->
	    {file, {atomize(FileName)}};
	{dets,do_open_file,[Tab,_FileName,_,_,_,_,_,_Ram,_,_,_]} ->
	    {dets, {Tab}};
	{application_master,start_it,[_,{state,_,ApplD,_,_,_},_,_]} ->
	    {appl_data,App,_,_,_,_,_,_,_} = ApplD,
	    {application_master, {App}};
	{erlang,apply,[Fun,[]]} when function(Fun) -> 
	    funi(Fun);
	{M,F,As} -> 
	    {M,F,length(As)}
    end.

atomize(FileName) ->
    list_to_atom(hd(reverse(string:tokens(FileName, "/")))).

funi(Fun) ->
    case erlang:fun_info(Fun, module) of
	{_, rpc} ->
	    case erlang:fun_info(Fun, env) of
		{_, [_, _, Pid, A, _F, _M]} when is_pid(Pid), is_list(A) ->
		    {rpc, {call_dummy, node(Pid)}};
		{_, [_, Pid, A, F, M]} when is_pid(Pid)->
		    {rpc, {call, node(Pid)}, {M, F, length(A)}};
		{_, [Pid, A, F, M]} when is_pid(Pid), is_list(A) ->
		    {rpc, {cast, node(Pid)}, {M, F, length(A)}};
		_X -> 
		    {rpc}
	    end;
	{_, Mod} -> 
	    case erlang:fun_info(Fun, pid) of
		{_, Pid} when is_pid(Pid) -> {'fun', {Mod, node(Pid)}};
		{_, X} -> {'fun', {Mod, X}}
	    end
    end.

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) ->
    {gen_server,Module};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) ->
    {gen_server,Module};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) ->
    {gen_fsm,Module};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
    {gen_fsm,Module};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event};
trans_init(M,F,A) ->
    {M,F,length(A)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ets_ins(Rec) -> ets_ins(?MODULE, Rec).
ets_ins(Tab, Rec) -> ets:insert(Tab, Rec).
ets_lup(Key) -> ets_lup(?MODULE, Key).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} ->[];
	[{Key, R}] -> R;
	R -> R
    end.
