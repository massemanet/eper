%%%-------------------------------------------------------------------
%%% File    : sysWatchdog.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description : 
%%%
%%% Created : 11 Mar 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(watchdog).

-export([start/0,stop/0]).

-import(error_logger,[info_report/1,error_report/1]).

-record(ld, {jailed=[]				%jailed pids
	     ,subscribers=[out()]	        %where to send our reports
	     ,triggers=default_triggers()       %{atom(Tag), fun/1->no|fun/1}
	     ,prfState				%prfTarg:subscribe return val
	     ,prfData				%last prf data
	     ,monData				%last system_monitor data
	     ,lines=5				%# of displayed procs
	    }).
%% 	     gc_trig=500,			%gc time [ms]
%% 	     heap_trig=1024*256,		%heap size [words]
%% 	     mem_hw_trig=200,			%beam proc size [MB]
%%	     cpu_trig=0.95}).			%cpu load [frac]

%% constants

default_triggers() -> [{sysMon,long_gc,500}	      %gc time [ms]
		       ,{sysMon,large_heap,1024*256} %heap size [words]
		       ,{prfSys,user,fun(X)->true=(X<0.95) end}
		       ,{prfSys,kernel,fun(X)->true=(X<0.5) end}
		       ,{prfSys,iowait,fun(X)->true=(X<0.3) end}
		      ].

timeout(restart) -> 5000;			%  5 sec
timeout(release) -> 3000.			%  3 sec
-define(max_jailed, 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> 
  Init = fun(D) -> fun() -> init(D) end end,       % mmm.... haskell....
  {Pid,Ref} = erlang:spawn_monitor(Init(self())), 
  receive
    {Pid,ok} -> erlang:demonitor(Ref,[flush]),Pid;
    {'DOWN',Ref,_,Pid,OldPid} -> OldPid
  end.

stop() -> 
  try ?MODULE ! stop
  after ok
  end.
  
init(Daddy) ->
  try register(?MODULE,self()), 
      Daddy ! {self(),ok}
  catch _:_ -> exit(whereis(?MODULE))
  end,
  LD = #ld{prfState=prfTarg:subscribe(node(),self(),[prfSys,prfPrc,prfNet])},
  start_monitor(LD#ld.triggers),
  loop(LD).

loop(LD) when ?max_jailed < length(LD#ld.jailed) ->
  %% we take a timeout when enough pids are jailed. conservative is good.
  erlang:start_timer(timeout(restart), self(), restart),
  stop_monitor(),
  flush(),
  loop(LD#ld{jailed=[]});
loop(LD) ->
  receive
    %% quit
    stop -> 
      ok;
    print_state ->
      print_term(group_leader(),LD),
      loop(LD);
    %% set configs
    {set_lines,N} when integer(N) -> 
      loop(LD#ld{lines = N});		        %number of displayed processes
    {set_trigger,{ID,Fun}} ->
      loop(LD#ld{triggers=new_triggers(LD#ld.triggers,ID,Fun)});
    %% data from prfTarg
    {{data,_},Data} ->
      loop(check_triggers(LD#ld{prfData=Data}));
    %% data from system_monitor
    {monitor,Pid,Tag,Data} ->
      loop(check_jailed(LD,Pid,Tag,Data));
    %% restarting after timeout
    {timeout, _, restart} -> 
      start_monitor(LD),
      loop(LD#ld{jailed=[]});
    %% release a pid from jail
    {timeout, _, {release, Pid}} ->
      loop(LD#ld{jailed = LD#ld.jailed--[Pid]});
    X ->
      error_report([{?MODULE,unexpected},X]),
      loop(LD)
  end.

start_monitor(Triggers) ->
  erlang:system_monitor(self(), sysmons(Triggers)),Triggers.

sysmons(Triggers) ->
  [{Tag,Val} || {sysMon,Tag,Val} <- Triggers] ++
    [busy_port,busy_dist_port].

stop_monitor() ->
  erlang:system_monitor(undefined).

new_triggers(Triggers,ID,Fun) -> 
  maybe_restart(ID,[{ID,Fun}|clean_triggers(Triggers,[ID])]).

maybe_restart({sysMon,_},Triggers) -> stop_monitor(),start_monitor(Triggers);
maybe_restart(_,Triggers) -> Triggers.

clean_triggers(Triggers,IDs) ->
  lists:dropwhile(fun({ID,_})->lists:member(ID,IDs) end, Triggers).

check_jailed(LD,Pid,Tag,Data) ->
  case lists:member(Pid, LD#ld.jailed) of
    true -> LD;
    false->
      erlang:start_timer(timeout(release), self(), {release, Pid}),
      NLD = LD#ld{monData=[{tag,Tag},{pid,Pid},{data,Data}],
	    jailed=[Pid|LD#ld.jailed]},
      report(NLD,{sysMon,Tag}),
      NLD
  end.

check_triggers(LD) ->
  {Triggered, NewTriggers} = check_triggers(LD#ld.triggers,LD#ld.prfData),
  [report(LD,Trig) || Trig <- Triggered],
  LD#ld{triggers=NewTriggers}.

check_triggers(Triggers,Data) -> 
  case check_triggers(Triggers,Data,[]) of
    [] -> {[],Triggers};
    Trigd -> IDs = [ID || {ID,_} <- Trigd],
	     {IDs,Trigd++clean_triggers(Triggers,IDs)}
  end.

check_triggers([],_,O) -> O;
check_triggers([T|Ts],Data,O) ->
  try check_triggers(Ts,Data,[check_trigger(T,Data)|O])
  catch _:_ -> check_triggers(Ts,Data,O)
  end.

check_trigger({{Section,Tag},T},Data) -> 
  {{Section,Tag},T(get_measurement([Section,Tag],Data))}.

report(LD,Trigger) ->
  Report = make_report(Trigger,LD),
  [Sub(Report) || Sub <- LD#ld.subscribers].

make_report({sysMon,_Tag},LD) ->
  expand_pids(get_measurement([sysMon],LD));
make_report(Trigger,LD) ->
  [{?MODULE,Trigger}|maybe_procs(LD)++generic_report(LD)].

generic_report(LD) ->
  get_measurement([prfSys],LD).

maybe_procs(#ld{lines = 0}) -> [];
maybe_procs(LD) -> 
  lists:sublist(get_measurement([prfPrc,reds],LD),LD#ld.lines).

get_measurement([sysMon|Tags],#ld{monData=Data}) -> get_measurement(Tags,Data);
get_measurement(Tags,#ld{prfData=Data}) -> get_measurement(Tags,Data);
get_measurement([T|Tags],Data) -> get_measurement(Tags,lks(T,Data));
get_measurement([],Data) -> Data.

info(Info) -> [].

data(Port) when port(Port) -> porti(Port);
data(Data) when list(Data) -> mnorm(Data).

mnorm([]) -> [];
mnorm([{T=heap_block_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([{T=mbuf_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([{T=stack_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([{T=heap_size,N}|R]) -> [{T,N/16#40000}|mnorm(R)];
mnorm([H|R]) -> [H|mnorm(R)].


expand_pids([]) -> [];
expand_pids([{pid,Pid}|Es]) when is_pid(Pid)-> pi(Pid)++expand_pids(Es);
expand_pids([E|Es]) -> [E|expand_pids(Es)].

p_info() -> [message_queue_len,current_function,initial_call,registered_name].
pi(Pid) -> lists:reverse(pi(Pid, p_info(), [{pid,Pid}])).
pi(_Pid, [], O) -> O;
pi(Pid, [Tag|Tags], O) ->
  case process_info(Pid, Tag) of
    undefined -> pi(Pid, Tags, O);
    [] -> pi(Pid, Tags, O);
    Val -> pi(Pid, Tags, [Val|O])
  end.

flush() ->    
  receive {monitor,_,_,_} -> flush()
  after 0 -> ok
  end.

norm([], _) -> [];
norm(L, Max) when Max =< 0 -> L;
norm([{Tag,X}|T], Max) when number(X) -> [{Tag,X/Max}|norm(T,Max)];
norm([{X,Tag}|T], Max) when number(X) -> [{X/Max,Tag}|norm(T,Max)].

-define(PORTIS,[name,id,connected,input,output]).
porti(Port) -> 
  [{port,Port}|porti(Port, ?PORTIS)].

porti(_Port,[]) -> [];
porti(Port,[Porti|Portis]) ->
  case erlang:port_info(Port,Porti) of
    undefined -> porti(Port,Portis);
    X -> [X|porti(Port,Portis)]
  end.

lks(Tag,List,Def) ->
  try lks(Tag,List)
  catch _ -> Def
  end.

lks(Tag,List) -> 
  try {value,{Tag,Val}} = lists:keysearch(Tag,1,List), Val
  catch _:_ -> throw({no_such_key,Tag})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out() -> fun(E)->print_term(group_leader(),E) end.

print_term(FD,Term) -> io:fwrite(FD," ~p~n",[expand_recs(Term)]).

expand_recs(List) when is_list(List) -> [expand_recs(L)||L<-List];
expand_recs(Tup) when is_tuple(Tup) -> 
  case tuple_size(Tup) of
    L when L < 1 -> Tup;
    L ->
      Fields = ri(element(1,Tup)),
      case L == length(Fields)+1 of
	false-> list_to_tuple(expand_recs(tuple_to_list(Tup)));
	true -> expand_recs(lists:zip(Fields,tl(tuple_to_list(Tup))))
      end
  end;
expand_recs(Term) -> Term.

ri(ld) -> record_info(fields,ld);
ri(_) -> [].
