%% -*- erlang-indent-level: 2 -*-
%%% Created : 17 Oct 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('watchdog_proxy').
-author('Mats Cronqvist').
-export([start/0,start/1,stop/0]).
-export([print_state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% boiler plate
-behaviour(gen_server).
-export([handle_call/3, handle_cast/2, handle_info/2, 
         init/1, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% boiler plate
init(X) -> {ok,do_init(X)}.
terminate(Reason,LD) -> do_terminate(Reason,LD).
code_change(_,LD,_) -> {ok,LD}.
handle_cast(In,LD) -> gen_safe(fun(Ld)->noreply(do_cast(In,Ld))end,LD).
handle_info(print_state,LD) -> print_term(expand_recs(LD)),noreply(LD);
handle_info(In,LD) -> gen_safe(fun(Ld)->noreply(do_info(In,Ld))end,LD).
handle_call(stop,_From,LD) -> {stop,shutdown,stopped,LD};
handle_call(print_state,_,LD) -> print_term(expand_recs(LD)),reply({ok,LD});
handle_call(In,_,LD) -> gen_safe(fun(Ld)->reply(do_call(Ld,In))end,LD).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> start([]).

start(Args) -> gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

stop() -> try gen_server:call(?MODULE,stop) 
	  catch exit:{noproc,_} -> not_started
	  end.

print_state() -> gen_server:call(?MODULE,print_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_safe(F, LD) ->
  try F(LD)
  catch _:R -> {stop,{R,erlang:get_stacktrace()},LD}
  end.

noreply(LD) -> {noreply,LD}.
reply({Reply,LD}) -> {reply,Reply,LD}.

print_term(Term) -> print_term(group_leader(),Term).
print_term(FD,Term) -> 
  case node(FD) == node() of
    true -> error_logger:info_report(Term);
    false-> io:fwrite(FD," ~p~n",[Term])
  end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the state
-record(ld,{args,acceptor}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this should be put in here by the compiler. or a parse_transform...
ri(ld) ->record_info(fields,ld);
ri(_) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% constants

sock_opts() -> [binary, {reuseaddr,true}, {active,false}].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_init(Args) -> #ld{args=Args, acceptor=accept(producer,56669,sock_opts())}.

do_terminate(_Reason,_LD) -> ok.

do_call(Msg,LD) -> print_term(Msg),{ok,LD}.

do_cast(Msg,LD) -> print_term(Msg),LD.

do_info(Msg,LD) -> print_term(Msg),LD.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accept is blocking, so it runs in its own process
accept(What,Port,Opts) ->
  erlang:spawn_link(fun() -> acceptor(What,Port,Opts) end).

acceptor(What,Port,Opts) ->
  {ok,ListenSock} = gen_tcp:listen(Port,Opts),
  acceptor_loop(What,ListenSock).

acceptor_loop(What,ListenSock) ->
  {ok,Socket} = gen_tcp:accept(ListenSock),
  ?MODULE ! {new_socket,What,Socket},
  gen_tcp:controlling_process(Socket,whereis(?MODULE)),
  acceptor_loop(What,ListenSock).
