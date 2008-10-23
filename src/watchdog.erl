%%%-------------------------------------------------------------------
%%% File    : watchdog.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description : 
%%%
%%% Created : 11 Mar 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(watchdog).

-export([start/0,stop/0]).
-export([send/3,out/1]).

-import(error_logger,[info_report/1,error_report/1]).

-record(ld, 
	{jailed=[]			    %jailed pids
	 ,subscribers=default_subs()        %where to send our reports
	 ,triggers=default_triggers()       %{atom(Tag), fun/1->no|fun/1}
	 ,prfState			    %prfTarg:subscribe return val
	 ,prfData			    %last prf data
	 ,monData			    %last system_monitor data
	 ,lines=5			    %# of displayed procs
	}).

%% constants

default_subs() -> 
  [
%   ,send("streamserver.kreditor.se",56669,"I'm a Cookie")
   send("sterlett",56669,"I'm a Cookie")
%   ,out(group_leader())
  ].

default_triggers() -> 
  [ {[sysMon,long_gc],500}	      %gc time [ms]
   ,{[sysMon,large_heap],1024*256} %heap size [words]
   ,{[ticker],true}
   ,{[prfSys,user],fun(X)->true=(0.95<X) end}
   ,{[prfSys,kernel],fun(X)->true=(0.5<X) end}
   ,{[prfSys,iowait],fun(X)->true=(0.3<X) end}
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
  catch _:_ -> ok
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
    %% fake trigger for debugging
    trigger ->
      report(LD,[test]),
      loop(LD);
    %% data from prfTarg
    {{data,_},Data} ->
      NLD = LD#ld{prfData=Data},
      loop(try do_triggers(check_jailed(NLD,prfData)) catch _:_ -> NLD end);
    %% data from system_monitor
    {monitor,Pid,Tag,Data} ->
      NLD = LD#ld{monData=[{tag,Tag},{pid,Pid},{data,Data}]},
      loop(try do_mon(check_jailed(NLD,Pid)) catch _:_ -> NLD end);
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
  [{Tag,Val} || {[sysMon,Tag],Val} <- Triggers] ++ [busy_port,busy_dist_port].

stop_monitor() ->
  erlang:system_monitor(undefined).

%% trigger() :: {ID,Val} 
%% Val :: number() | function()
%% ID :: list(Tag) 
%% Tag :: atom() - tags into the data structure. i.e. [prfSys,iowait]
new_triggers(Triggers,ID,Fun) -> 
  maybe_restart(hd(ID),[{ID,Fun}|clean_triggers(Triggers,[ID])]).

maybe_restart(sysMon,Triggers) -> stop_monitor(),start_monitor(Triggers);
maybe_restart(_,Triggers) -> Triggers.

clean_triggers(Triggers,IDs) ->
  lists:dropwhile(fun({ID,_})->lists:member(ID,IDs) end, Triggers).

%%exit if What is jailed
check_jailed(LD,What) ->
  false = lists:member(What, LD#ld.jailed),
  erlang:start_timer(timeout(release), self(), {release, What}),
  LD#ld{jailed=[What|LD#ld.jailed]}.

do_mon(LD) ->
  report(LD,[sysMon]),
  LD.

do_triggers(LD) ->
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
  check_triggers(Ts,Data,try [check_trigger(T,Data)|O] catch _:_-> O end).

check_trigger({ID,true},_Data) -> {ID,true};
check_trigger({ID,T},Data) -> 
  case T(get_measurement(ID,Data)) of
    true -> {ID,T};
    F when is_function(F)  -> {ID,F}
  end.

report(LD,Trigger) ->
  Report = make_report(Trigger,LD),
  [Sub(Report) || Sub <- LD#ld.subscribers].

make_report([sysMon|_],LD) ->
  [{?MODULE,sysMon}|expand_ps(LD#ld.monData)];
make_report(Trigger,LD) ->
  [{?MODULE,Trigger}|maybe_procs(LD)++generic_report(LD#ld.prfData)].

generic_report(LD) ->
  get_measurement([prfSys],LD).

maybe_procs(#ld{lines = 0}) -> [];
maybe_procs(LD) -> 
  lists:sublist(get_measurement([prfPrc,reds],LD#ld.prfData),LD#ld.lines).

get_measurement([T|Tags],Data) -> get_measurement(Tags,lks(T,Data));
get_measurement([],Data) -> Data.

expand_ps([]) -> [];
expand_ps([{T,P}|Es]) when is_pid(P)-> pii({T,P})++expand_ps(Es);
expand_ps([{T,P}|Es]) when is_port(P)-> poi({T,P})++expand_ps(Es);
expand_ps([E|Es]) -> [E|expand_ps(Es)].

pi_info() -> [message_queue_len,current_function,initial_call,registered_name].
pii({T,Pid}) -> [{T,Pid} | pii(Pid, pi_info())].
pii(_Pid, []) -> [];
pii(Pid, [Tag|Tags]) ->
  case process_info(Pid, Tag) of
    undefined -> pii(Pid, Tags);
    [] -> pii(Pid, Tags);
    Val -> [Val|pii(Pid, Tags)]
  end.

po_info() -> [name,id,connected,input,output].
poi({T,Port}) -> [{T,Port}|poi(Port, po_info())].
poi(_Port,[]) -> [];
poi(Port,[Porti|Portis]) ->
  case erlang:port_info(Port,Porti) of
    undefined -> poi(Port,Portis);
    X -> [X|poi(Port,Portis)]
  end.

flush() ->    
  receive {monitor,_,_,_} -> flush()
  after 0 -> ok
  end.

lks(Tag,List) -> 
  try {value,{Tag,Val}} = lists:keysearch(Tag,1,List), Val
  catch _:_ -> throw({no_such_key,Tag})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send(Name,Port,Cookie) ->
  fun(Chunk)->
      try {ok,Sck} = gen_tcp:connect(Name,Port,conn_opts(),conn_timeout()),
	  try gen_tcp:send(Sck,prf_crypto:encrypt(Cookie,expand_recs(Chunk)))
	  after gen_tcp:close(Sck)
	  end
      catch _:_ -> ok
      end
  end.
conn_opts() -> [{send_timeout,1000},{active,false},{packet,4},binary].
conn_timeout() -> 1000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(FD) -> fun(E)-> print_term(FD,expand_recs(E)) end.

%%print_term(Term) -> print_term(group_leader(),Term).
print_term(FD,Term) -> 
  case node(FD) == node() of
    true -> error_logger:info_report(Term);
    false-> io:fwrite(FD," ~p~n",[Term])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
