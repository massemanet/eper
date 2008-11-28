%%%-------------------------------------------------------------------
%%% File    : watchdog.erl
%%% Author  : Mats Cronqvist <etxmacr@mwux005>
%%% Description : 
%%%
%%% Created : 11 Mar 2005 by Mats Cronqvist <etxmacr@mwux005>
%%%-------------------------------------------------------------------
-module(watchdog).

-export([start/0,stop/0]).
-export([add_send_subscriber/3,add_log_subscriber/0,clear_subscribers/0]).
-export([message/1]).
-export([loop/1]).

-include("log.hrl").

-record(ld, 
	{jailed=[]			 %jailed pids
	 ,subscribers=[]		 %where to send our reports
	 ,triggers=default_triggers()	 %{atom(Tag), fun/1->no|fun/1}
	 ,prfState			 %prfTarg:subscribe return val
	 ,userData			 %last user data
	 ,prfData			 %last prf data
	 ,monData			 %last system_monitor data
	}).

%% constants

default_triggers() -> 
  [ {[sysMon,long_gc],500}	      %gc time [ms]
   ,{[sysMon,large_heap],1024*1024}    %heap size [words]
   ,{user,true}
   ,{ticker,true}
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

%% E.g:  add_send_subscriber("localhost",56669,"I'm a Cookie").
add_send_subscriber(Host,Port,PassPhrase) ->
  try {ok,{hostent,Host,[],inet,4,_}} = inet:gethostbyname(Host),
      ?MODULE ! {add_subscriber,mk_send(Host,Port,PassPhrase)},
      ok
  catch 
    error:{badmatch,{ok,{hostent,G,[W],inet,4,_}}} -> {error,{badhost,W,G}};
    _:R -> {error,R}
  end.

add_log_subscriber() ->
  try ?MODULE ! {add_subscriber,mk_log(group_leader())}, ok
  catch _:R -> {error,R}
  end.

clear_subscribers() ->
  try ?MODULE ! clear_subscribers, ok
  catch _:R -> {error,R}
  end.

message(Term) ->
  try ?MODULE ! {user,Term}, ok
  catch C:R -> {C,R}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Daddy) ->
  try register(?MODULE,self()), 
      Daddy ! {self(),ok}
  catch _:_ -> exit(whereis(?MODULE))
  end,
  LD = #ld{prfState=prfTarg:subscribe(node(),self(),[prfSys,prfPrc])},
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
    reload ->
      ?MODULE:loop(LD);
    print_state ->
      print_term(group_leader(),LD),
      loop(LD);
    %% set configs
    list_triggers ->
      print_term(group_leader(),LD#ld.triggers),
      loop(LD);
    {del_trigger,ID} ->
      loop(LD#ld{triggers=del_trigger(LD#ld.triggers,ID)});
    {add_trigger,{ID,Fun}} ->
      loop(LD#ld{triggers=add_trigger(LD#ld.triggers,ID,Fun)});
    list_subscribers ->
      print_term(group_leader(),LD#ld.subscribers),
      loop(LD);
    clear_subscribers ->
      loop(LD#ld{subscribers=[]});
    {add_subscriber,Sub} ->
      loop(LD#ld{subscribers=[Sub|LD#ld.subscribers]});
    %% fake trigger for debugging
    trigger ->
      send_report(LD,test),
      loop(LD);
    %% data from user
    {user,Data} -> 
      NLD = LD#ld{userData=Data},
      loop(try do_user(check_jailed(NLD,userData)) catch _:_ -> NLD end);
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
      start_monitor(LD#ld.triggers),
      loop(LD#ld{jailed=[]});
    %% release a pid from jail
    {timeout, _, {release, Pid}} ->
      loop(LD#ld{jailed = LD#ld.jailed--[Pid]});
    X ->
      ?log({unexpected_msg,X}),
      loop(LD)
  end.

start_monitor(Triggers) ->
  erlang:system_monitor(self(), sysmons(Triggers)).

sysmons(Triggers) ->
  [{Tag,Val} || {[sysMon,Tag],Val} <- Triggers] ++ [busy_port,busy_dist_port].

stop_monitor() ->
  erlang:system_monitor(undefined).

%% trigger() :: {ID,Val} 
%% Val :: number() | function()
%% ID :: list(Tag) 
%% Tag :: atom() - tags into the data structure. i.e. [prfSys,iowait]
add_trigger(Triggers,ID,Fun) -> 
  maybe_restart(ID,[{ID,Fun}|clean_triggers(Triggers,[ID])]).

del_trigger(Triggers,ID) ->
  maybe_restart(ID,clean_triggers(Triggers,[ID])).

maybe_restart(Trig,Triggers) -> 
  case Trig of 
    [sysMon|_] -> stop_monitor(), start_monitor(Triggers);
    _ -> ok
  end,
  Triggers.

clean_triggers(Triggers,IDs) ->
  lists:filter(fun({ID,_})->not lists:member(ID,IDs) end, Triggers).

%%exit if What is jailed
check_jailed(LD,What) ->
  false = lists:member(What, LD#ld.jailed),
  erlang:start_timer(timeout(release), self(), {release, What}),
  LD#ld{jailed=[What|LD#ld.jailed]}.

do_mon(LD) ->
  send_report(LD,sysMon),
  LD.

do_user(LD) ->
  send_report(LD,user),
  LD.

do_triggers(LD) ->
  {Triggered, NewTriggers} = check_triggers(LD#ld.triggers,LD#ld.prfData),
  [send_report(LD,Trig) || Trig <- Triggered],
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

check_trigger({ticker,true},_Data) -> {ticker,true};
check_trigger({ID,T},Data) when is_function(T) -> 
  case T(get_measurement(ID,Data)) of
    true -> {ID,T};
    NewT when is_function(NewT)  -> {ID,NewT}
  end.

send_report(LD,Trigger) ->
  Report = make_report(Trigger,LD),
  [Sub(Report) || Sub <- LD#ld.subscribers].

make_report(user,LD) ->
  [{?MODULE,user},{userData,LD#ld.userData}];
make_report(sysMon,LD) ->
  [{?MODULE,sysMon}|expand_ps(LD#ld.monData)];
make_report(Trigger,LD) ->
  [{?MODULE,Trigger}|generic_report(LD)].

generic_report(LD) ->
  LD#ld.prfData.

expand_ps([]) -> [];
expand_ps([{T,P}|Es]) when is_pid(P)-> pii({T,P})++expand_ps(Es);
expand_ps([{T,P}|Es]) when is_port(P)-> poi({T,P})++expand_ps(Es);
expand_ps([E|Es]) -> [E|expand_ps(Es)].

pi_info() -> [message_queue_len,current_function,initial_call,registered_name].
pii({T,Pid}) -> [{T,Pid} | prfPrc:pid_info(Pid, pi_info())].

poi({T,Port}) -> 
  {Name,Stats} = prfNet:port_info(Port),
  [{T,Port},{name,Name}|Stats].

flush() ->    
  receive {monitor,_,_,_} -> flush()
  after 0 -> ok
  end.

get_measurement([T|Tags],Data) -> get_measurement(Tags,lks(T,Data));
get_measurement([],Data) -> Data.

lks(Tag,List) -> 
  try {value,{Tag,Val}} = lists:keysearch(Tag,1,List), Val
  catch _:_ -> throw({no_such_key,Tag})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_send(Name,Port,Cookie) ->
  fun(Chunk)->
      try {ok,Sck} = gen_tcp:connect(Name,Port,conn_opts(),conn_timeout()),
	  try gen_tcp:send(Sck,prf_crypto:encrypt(Cookie,expand_recs(Chunk)))
	  after gen_tcp:close(Sck)
	  end
      catch _:_ -> ok
      end
  end.
conn_opts() -> [{send_timeout,100},{active,false},{packet,4},binary].
conn_timeout() -> 100.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_log(FD) -> fun(E)-> print_term(FD,expand_recs(E)) end.

print_term(FD,Term) -> 
  case node(FD) == node() of
    true -> ?log(Term);
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
