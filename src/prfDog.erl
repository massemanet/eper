%% -*- erlang-indent-level: 2 -*-
%%% Created : 17 Oct 2008 by Mats Cronqvist <masse@kreditor.se>

%% implements a proxy function between watchdog and the prf consumers.

-module(prfDog).
-author('Mats Cronqvist').

%% prf callbacks
-export([collect/1,config/2]).

% gen_serv callbacks
-export(
   [handle_info/2
    ,handle_call/3
    ,init/1
    ,rec_info/1]).

-include("log.hrl").

-record(ld,{args,acceptor,socket=[],msg=orddict:new(),cookie="I'm a Cookie"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prf callbacks, runs in the prfTarg process

collect(init) ->
  gen_serv:start(?MODULE),
  {gen_serv:get_state(?MODULE),{?MODULE,[]}};
collect(LD) ->
  {LD,{?MODULE,gen_server:call(?MODULE,get_data)}}.

config(LD,Data) ->
  ?log([unknown,{data,Data}]),
  LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_serv callbacks

rec_info(ld) -> record_info(fields,ld);
rec_info(_)  -> [].

init(Args) ->
  LD = #ld{args=Args,acceptor=accept(56669)},
  watchdog:add_send_subscriber(tcp,"localhost",56669,LD#ld.cookie),
  LD.

handle_call(get_data,_,LD) ->
  {LD#ld.msg,LD#ld{msg=orddict:new()}}.

handle_info({new_socket,Sock},LD) ->
  %% we accepted a socket towards a producer.
  LD#ld{socket=[Sock|LD#ld.socket]};
handle_info({tcp,Sock,Bin},LD) ->
  case lists:member(Sock,LD#ld.socket) of
    true ->
      %% got data from a known socket. this is good
      {watchdog,Node,TS,Trig,Msg} = prf_crypto:decrypt(LD#ld.cookie,Bin),
      LD#ld{socket=LD#ld.socket,
            msg=orddict:store({Node,TS,Trig},Msg,LD#ld.msg)};
    false->
      %% got data from unknown socket. wtf?
      ?log([{data_from,Sock},{sockets,LD#ld.socket},{bytes,byte_size(Bin)}]),
      LD
  end;
handle_info({tcp_closed, Sock},LD) ->
  LD#ld{socket=LD#ld.socket--[Sock]};
handle_info({tcp_error, Sock, Reason},LD) ->
  ?log([{tcp_error,Reason},{socket,Sock}]),
  LD;
handle_info(Msg,LD) ->
  ?log([{unrec,Msg}]),
  LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
