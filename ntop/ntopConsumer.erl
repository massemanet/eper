%%%----------------------------------------------------------------
%%% File        : ntopConsumer.erl
%%% Author      : Mats Cronqvist <locmacr@mwlx084>
%%% Created     : 12 Feb 2007
%%% Description : 
%%%----------------------------------------------------------------
-module(ntopConsumer).
-author('Mats Cronqvist').

-export([init/1, terminate/1, tick/2, collectors/0, config/2]).

-import(orddict,[from_list/1,fetch/2,new/0,store/3,fold/3]).

-record(ld,{node,prfNet=[],prfSys=[]}).

collectors() -> [prfNet,prfSys].
init(Node) -> #ld{node = Node}.
terminate(_LD) -> ok.

config(LD,_) -> LD.

tick(LD,Data) ->
  case Data of
    [] -> LD;
    [{prfNet,PrfNet},{prfSys,PrfSys}] -> 
      Net = from_list(PrfNet),
      Sys = from_list(PrfSys),
      print(LD,Sys,Net),
      LD#ld{prfSys=Sys,prfNet=Net}
  end.

print(#ld{prfNet=OPrfNet},_,PrfNet) ->
  print_header(PrfNet),
  [print(Name,Data) || {Name,Data} <- net(PrfNet,OPrfNet)].

print_header([{_Name,Data}|_])->  
  io:fwrite("~s",[lists:flatten([str("~n~-19w",[name]),
                                 [str("~6w",[short(I)])|| {I,_V}<-Data, ok(I)],
                                 str("~n",[])])]).

print(Name,Data) ->
  io:fwrite("~s",[lists:flatten([str("~-19s",[name(Name)]),
                                 [str("~6w", [V]) || {I,V}<-Data, ok(I)],
                                 str("~n",[])])]).

net(NetN,NetO) -> 
  {_,O} = fold(fun foldf/3,{NetO,new()},NetN),
  O.

foldf(Key,Val,{KVs,O}) ->
  try {KVs,store(Key,zipsub(Val,fetch(Key,KVs)),O)}
  catch _:_ -> {KVs,O}
  end.

zipsub([],[]) -> [];
zipsub([{K,V1}|R1],[{K,V2}|R2]) -> [{K,V1-V2}|zipsub(R1,R2)].

str(F,A) -> io_lib:fwrite(F,A).

name({tcp,{Host,Port}}) -> Host++[$:|integer_to_list(Port)];
name({udp,Port}) -> [$:|integer_to_list(Port)];
name({node,NodeName}) -> NodeName;
name(X) -> X.

ok(recv_oct) -> true;
ok(recv_cnt) -> true;
ok(send_oct) -> true;
ok(send_cnt) -> true;
ok(send_pend)-> true;
ok(input)    -> true;
ok(output)   -> true;
ok(send_max) -> true;
ok(recv_max) -> true;
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
