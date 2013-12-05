%%%----------------------------------------------------------------
%%% File        : ntopConsumer.erl
%%% Author      : Mats Cronqvist <locmacr@mwlx084>
%%% Created     : 12 Feb 2007
%%% Description : net-top consumer
%%%----------------------------------------------------------------
-module(ntopConsumer).
-author('Mats Cronqvist').

-export([init/1, terminate/1, tick/2, collectors/0, config/2]).

-record(ld,{node,prfNet=[],prfSys=[]}).

collectors() -> [prfNet,prfSys].
init(Node) -> #ld{node = Node}.
terminate(_LD) -> ok.

config(LD,_) -> LD.

tick(LD,Data) ->
  case Data of
    [{prfNet,PrfNet},{prfSys,PrfSys}] -> print(LD, PrfNet, PrfSys);
    _ -> LD
  end.

print(LD, PrfNet, PrfSys) ->
  Net = orddict:from_list(PrfNet),
  Sys = orddict:from_list(PrfSys),
  print(LD#ld.prfNet,Net),
  LD#ld{prfSys=Sys,prfNet=Net}.

print(OPrfNet,PrfNet) ->
  print_header(PrfNet),
  [print_port(Name,Data) || {Name,Data} <- net(PrfNet,OPrfNet)].

print_header(NDs)->
  [{_Name,Data}|_] = lists:reverse(NDs),
  io:fwrite("~s",[lists:flatten([str("~n~-23w",[name]),
                                 [str("~8w",[short(I)])|| {I,_V}<-Data, ok(I)],
                                 str("~n",[])])]).

print_port(Name,Data) ->
  io:fwrite("~s",[lists:flatten([str("~-23s",[name(Name)]),
                                 [str("~8w", [V]) || {I,V}<-Data, ok(I)],
                                 str("~n",[])])]).

net(NetN,NetO) ->
  {_,O} = orddict:fold(fun foldf/3,{NetO,orddict:new()},NetN),
  O.

foldf(Key,Val,{KVs,O}) ->
  try {KVs,orddict:store(Key,zipsub(Val,orddict:fetch(Key,KVs)),O)}
  catch _:_ -> {KVs,O}
  end.

zipsub([],[]) -> [];
zipsub([{K,V1}|R1],[{K,V2}|R2]) when is_integer(V1),is_integer(V2)->
  [{K,V1-V2}|zipsub(R1,R2)];
zipsub([{K,V}|R1],[{K,V}|R2]) -> [{K,V}|zipsub(R1,R2)].

str(F,A) -> io_lib:fwrite(F,A).

name({tcp,{Host,Port}}) -> Host++[$:|integer_to_list(Port)];
name({udp,Port}) -> [$:|integer_to_list(Port)];
name({node,NodeName}) -> NodeName;
name({driver,Dr}) -> Dr;
name(X) -> io_lib:format("~p", [X]).

ok(recv_oct) -> true;
ok(recv_cnt) -> true;
ok(send_oct) -> true;
ok(send_cnt) -> true;
ok(send_pend)-> true;
ok(input)    -> true;
ok(output)   -> true;
ok(_)        -> false.

short(recv_oct) -> r_oct;
short(recv_cnt) -> r_cnt;
short(recv_max) -> r_max;
short(recv_avg) -> r_avg;
short(recv_dvi) -> r_dvi;
short(send_oct) -> s_oct;
short(send_cnt) -> s_cnt;
short(send_max) -> s_max;
short(send_avg) -> s_avg;
short(send_pend)-> s_pnd;
short(input)    -> in;
short(output)   -> out.
