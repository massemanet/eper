%% -*- erlang-indent-level: 2 -*-
%%% Created :  3 Nov 2008 by masse <masse@kreditor.se>

%% defines all the gen_server boilerplate

-author('Mats Cronqvist').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server boilerplate
-behaviour(gen_server).
-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_serv API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0,start/1,stop/0]).
-export([print_state/0]).

-include("log.hrl").

ri(ld) -> record_info(fields,ld);
ri(_) -> [].

start() -> start([]).

start(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
  try gen_server:call(?MODULE,stop)
  catch exit:{noproc,_} -> not_started
  end.

print_state() -> gen_server:call(?MODULE,print_state).

%% gen_server callbacks
init(X) -> ok(do_init(X)).
terminate(Reason,LD) -> do_terminate(LD,Reason).
code_change(_,LD,X) -> gen_safe(fun(Ld)->ok(do_code_change(Ld,X))end,LD).
handle_cast(In,LD) -> gen_safe(fun(Ld)->noreply(do_cast(Ld,In))end,LD).
handle_info(print_state,LD) -> print_term(expand_recs(LD)),noreply(LD);
handle_info(In,LD) -> gen_safe(fun(Ld)->noreply(do_info(Ld,In))end,LD).
handle_call(stop,_From,LD) -> {stop,shutdown,stopped,LD};
handle_call(print_state,_,LD) -> print_term(expand_recs(LD)),reply({ok,LD});
handle_call(In,_,LD) -> gen_safe(fun(Ld)->reply(do_call(Ld,In))end,LD).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_safe(F, LD) ->
  try F(LD)
  catch _:R -> {stop,{R,erlang:get_stacktrace()},LD}
  end.

ok(X) -> {ok,X}.
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
