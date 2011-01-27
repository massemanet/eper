%% -*- erlang-indent-level: 2 -*-
%%% Created : 17 Oct 2008 by Mats Cronqvist <masse@kreditor.se>

%% implements a proxy function between watchdog and the prf consumers.

-module(prfDog).
-author('Mats Cronqvist').

%% prf callbacks
-export([collect/1,config/2]).

-import(orddict,[new/0,store/3]).

-record(ld,{args,acceptor,socket=[],msg=new(),cookie="I'm a Cookie"}).
-include("gen_serv.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prf callbacks, runs in the prfTarg process

collect(LD) ->
  assert(),
  {LD,{?MODULE,gen_server:call(?MODULE,get_data)}}.

config(LD,Data) ->
  ?log([unknown,{data,Data}]),
  LD.

assert() ->
  case whereis(?MODULE) of
    undefined -> start();
    _ -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% constants

sock_opts() -> [binary, {reuseaddr,true}, {active,false}, {packet,4}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user code

do_init(Args) ->
  #ld{args=Args, acceptor=accept(producer,56669,sock_opts())}.

do_terminate(_LD,_Reason) -> ok.

do_code_change(LD,_Xtra) -> LD.

do_call(LD,get_data) -> {LD#ld.msg,LD#ld{msg=new()}};
do_call(LD,Msg) -> print_term(Msg),{ok,LD}.

do_cast(LD,Msg) -> print_term(Msg),LD.

do_info(LD,{new_socket,producer,Sock}) ->
  %% we accepted a socket towards a producer.
  inet:setopts(Sock,[{active,once}]),
  LD#ld{socket=[Sock|LD#ld.socket]};
do_info(LD,{tcp,Sock,Bin}) ->
  case lists:member(Sock,LD#ld.socket) of
    true ->
      %% got data from a known socket. this is good
      gen_tcp:close(Sock),
      {watchdog,Node,Trig,Msg} = prf_crypto:decrypt(LD#ld.cookie,Bin),
      LD#ld{socket=LD#ld.socket--[Sock],msg=store({Node,Trig},Msg,LD#ld.msg)};
    false->
      %% got data from unknown socket. wtf?
      ?log([{data_from,Sock},{sockets,LD#ld.socket},{bytes,byte_size(Bin)}]),
      LD
  end;
do_info(LD,{tcp_closed, Sock}) ->
  LD#ld{socket=LD#ld.socket--[Sock]};
do_info(LD,{tcp_error, Sock, Reason}) ->
  ?log([{tcp_error,Reason},{socket,Sock}]),
  LD;
do_info(LD,Msg) ->
  ?log([{unrec,Msg}|expand_recs(LD)]),
  LD.

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
