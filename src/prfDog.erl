%% -*- erlang-indent-level: 2 -*-
%%% Created : 17 Oct 2008 by Mats Cronqvist <masse@kreditor.se>

%% implements a proxy function between watchdog and the prf consumers.

-module(prfDog).
-author('Mats Cronqvist').

%% prf callbacks
-export([collect/1,config/2]).

-behaviour(gen_server).
-export([init/1,terminate/2,code_change/3,
         handle_call/3,handle_cast/2,handle_info/2]).

-export([state/0,quit/0]).

state() ->
  gen_server:call(?MODULE,state).

quit() ->
  gen_server:call(?MODULE,quit).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prf callbacks, runs in the prfTarg process

collect(init) ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]),
  {[],{?MODULE,[]}};
collect(LD) ->
  {LD,{?MODULE,gen_server:call(?MODULE,get_data)}}.

config(ports_opened,{port,Port}) when is_integer(Port) ->
  gen_server:call(?MODULE,quit),
  Secret = proplists:get_value(secret,gen_server:call(?MODULE,state)),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]),
  config(config([],{secret,Secret}),{port,Port});
config([],{port,Port}) when is_integer(Port) ->
  gen_server:call(?MODULE,{config,{port,Port}}),
  ports_opened;
config(LD,{secret,Secret}) when is_list(Secret) ->
  gen_server:call(?MODULE,{config,{secret,Secret}}),
  LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks

%% boilerplate
terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

handle_cast(_What,State) ->
  {noreply,State}.

%% not boilerplate
-record(ld,{port,
            secret,
            msg=[],
            acceptor,
            udp_socket,
            tcp_sockets=[]}).

init([]) ->
  {ok,#ld{}}.

handle_call(quit,_,LD) ->
  catch exit(LD#ld.acceptor,kill),
  catch gen_udp:close(LD#ld.udp_socket),
  {stop,normal,stopping,#ld{}};
handle_call(state,_,LD) ->
  Fields = record_info(fields,ld),
  {reply,lists:zip(Fields,tl(tuple_to_list(LD))),LD};
handle_call({config,{port,Port}},_,LD) ->
  {reply,[],LD#ld{port=Port,acceptor=accept(Port),udp_socket=udp_open(Port)}};
handle_call({config,{secret,Secret}},_,LD) ->
  {reply,[],LD#ld{secret=Secret}};
handle_call(get_data,_,LD) ->
  {reply,LD#ld.msg,LD#ld{msg=[]}}.

handle_info({new_socket,Sock},LD) ->
  %% we accepted a socket towards a producer.
  {noreply,LD#ld{tcp_sockets=[Sock|LD#ld.tcp_sockets]}};
handle_info({tcp,Sock,Bin},LD) ->
  case lists:member(Sock,LD#ld.tcp_sockets) of
    true ->
      %% got data from a known socket. this is good
      {noreply,decrypt(Bin,LD)};
    false->
      %% got data from unknown socket. wtf?
      ?log([{data_from,Sock},{bytes,byte_size(Bin)}]),
      {noreply,LD}
  end;
handle_info({tcp_closed, Sock},LD) ->
  case lists:member(Sock,LD#ld.tcp_sockets) of
    true ->
      {noreply,LD#ld{tcp_sockets=LD#ld.tcp_sockets--[Sock]}};
    false ->
      ?log([{unknown_socket_exited,Sock}]),
      {noreply,LD}
  end;
handle_info({tcp_error, Sock, Reason},LD) ->
  ?log([{tcp_error,Reason},{socket,Sock}]),
  {noreply,LD};
handle_info({udp,Socket,_IP,_Port,Bin},LD) ->
  case Socket == LD#ld.udp_socket of
    true ->
      inet:setopts(Socket,[{active,once}]),
      {noreply,decrypt(Bin,LD)};
    false ->
      %% got data from unknown socket. wtf?
      ?log([{unknown_socket,Socket},{bytes,byte_size(Bin)}]),
      {noreply,LD}
  end;
handle_info(Msg,LD) ->
  ?log([{unrec,Msg}]),
  {noreply,LD}.

decrypt(Bin,LD) ->
  case LD#ld.secret of
    undefined ->
      ?log([{no_secret}]),
      LD;
    Secret ->
      try
        <<PaySize:32,Payload/binary>> = Bin,
        PaySize = byte_size(Payload),
        B = prf_crypto:decrypt(Secret,Payload),
        {watchdog,Node,TS,Trig,Msg} = binary_to_term(B),
        LD#ld{msg=[{Node,TS,Trig,Msg}|LD#ld.msg]}
      catch
        _:R ->
          ?log({decrypt_failed,R}),
          LD
      end
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UPD socket
udp_open(Port) ->
  {ok,Socket} = gen_udp:open(Port,
                             [binary,
                              {recbuf,1024*1024},
                              {reuseaddr, true},
                              {active, once}]),
  Socket.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TCP socket
%% accept is blocking, so it runs in its own process
accept(Port) ->
  erlang:spawn_link(fun() -> acceptor(Port) end).

acceptor(Port) ->
  Opts = [binary,{reuseaddr,true},{active,true},{packet,4}],
  {ok,ListenSock} = gen_tcp:listen(Port,Opts),
  acceptor_loop(ListenSock).

acceptor_loop(ListenSock) ->
  {ok,Socket} = gen_tcp:accept(ListenSock),
  ?MODULE ! {new_socket,Socket},
  gen_tcp:controlling_process(Socket,whereis(?MODULE)),
  acceptor_loop(ListenSock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

t0_test() ->
  net_kernel:start([wdt,shortnames]),
  Port = 16#dada,
  Secret = "PWD",
  prf:start (dogC,node(),dogConsumer,node()),
  prf:config(dogC,prfDog,{secret,Secret}),
  prf:config(dogC,prfDog,{port,Port}),
  watchdog:start(),
  watchdog:delete_triggers(),
  watchdog:add_trigger(user, true),
  watchdog:add_send_subscriber(16#babe,"localhost",Port,Secret),
  watchdog:message(troglodyte),
  watchdog:stop(),
  poll(),
  ?assertMatch([{_,_,user,troglodyte}],
               prf:stop(dogC)).

poll() ->
  case prf:state(dogC) of
    {ld,[]} -> receive after 300 -> ok end,
               poll();
    _ -> ok
  end.

-endif.
