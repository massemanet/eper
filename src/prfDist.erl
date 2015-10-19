%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : prfDist.erl
%%% Author  : Daniel Szoboszlay <daniel.szoboszlay@klarna.com>
%%% Description : prf collector of distribution ports info
%%%
%%% Created : 16 Oct 2015 by Daniel Szoboszlay <daniel.szoboszlay@klarna.com>
%%%-------------------------------------------------------------------
-module(prfDist).

%% prf collector API
-export([collect/1,config/2]).

-include_lib("kernel/include/inet.hrl").

-record(node_info, { recv_cnt  = 0
                   , send_cnt  = 0
                   , send_pend = 0
                   , last_recv = {0, 0, 0}
                   , last_send = {0, 0, 0}
                   }).
%% Internal information kept in the collector's state on each
%% distribution port.

-type node_info() :: #node_info{ recv_cnt  :: non_neg_integer()
                               , send_cnt  :: non_neg_integer()
                               , last_recv :: erlang:timestamp()
                               , last_send :: erlang:timestamp()
                               }.
%% The type of internal information kept on each distribution port.

-type maybe_state() :: init | state().
%% The (optionally uninitialised) internal state of the module.

-type state() :: [{node(), node_info()}].
%% The internal state of the module, an orddict.

-type acc() :: { state()
               , MaxRecvIdle :: non_neg_integer()
               , MaxSendIdle :: non_neg_integer()
               , [node_stat()]
               }.
%% Type of an accumulator used when calculating statistics and
%% updating the internal state of the collector.

-type data() :: {Id :: module(), stats()}.
%% Data collected by the module.

-type stats() :: [node_stat() | top_stat()].
%% Statistics collected by the module.

-type node_stat() :: {node(), [port_stat() | idle_stat()]}.
%% Statistics collected from a distribution port towards a specific node.

-type top_stat()  :: {top, [idle_stat()]}.
%% Statistics on the maximum idle time of distribution ports.

-type port_stat() :: {recv_cnt,  non_neg_integer()} % received packets
                   | {send_cnt,  non_neg_integer()} % sent packets
                   | {send_pend, non_neg_integer()} % pending bytes
                   | idle_stat()
                     .
%% Statistics collected from a distribution port.

-type idle_stat() :: {recv_idle, non_neg_integer()} % no data received (in ms)
                   | {send_idle, non_neg_integer()} % no data sent (in ms)
                     .
%% Statistics on distribution port's idle time.

%% ==========================================================================
%% prf collector API
%% ==========================================================================

-spec collect(maybe_state()) -> {state(), data()}.
%% @private Collect information on distribution ports.
collect(init) ->
  collect(new_state());
collect(State) ->
  {NewState, Stats} =
    try erlang:system_info(dist_ctrl) of
        NodesPorts -> collect_stats(State, NodesPorts)
    catch
      _:_ -> {State, []}
    end,
  {NewState, {?MODULE, Stats}}.

-spec config(maybe_state(), _) -> state().
%% @private Update the configuration of the collector.
config(init,  ConfigData)  -> config(new_state(), ConfigData);
config(State, _ConfigData) -> State.

%% ==========================================================================
%% Internal functions
%% ==========================================================================

-spec new_state() -> state().
%% @doc Get an empty state.
new_state() ->
  orddict:new().

-spec collect_stats(State :: state(), NodesPorts :: [{node(), port()}]) ->
                       {state(), stats()}.
%% @doc Collect statistics from the distribution
collect_stats(State, NodesPorts) ->
  Now = os:timestamp(),
  {NewState, RecvIdle, SendIdle, NodeStats} =
    lists:foldl(fun ({Node, Port}, Acc) ->
                    collect_stats(Node, Port, State, Now, Acc)
                end,
                {new_state(), 0, 0, []},
                NodesPorts),
  Stats = [{top, [{recv_idle, RecvIdle}, {send_idle, SendIdle}]} | NodeStats],
  {NewState, Stats}.

-spec collect_stats(Node  :: node(),
                    Ctrl  :: port() | pid(),
                    State :: state(),
                    Now   :: erlang:timestamp(),
                    Acc   :: acc()) -> acc().
%% @doc Collect statistics from a given node's distribution port.
collect_stats(Node, Port, State, Now, Acc) when is_port(Port) ->
  try inet:getstat(Port, [recv_cnt, send_cnt, send_pend]) of
      {ok, PortStats} -> update_stats(Node, orddict:find(Node, State),
                                      PortStats, Now, Acc)
  catch
    _:_ ->
      %% Port got closed in the meantime?
      Acc
  end;
collect_stats(_Node, _Pid, _State, _Now, Acc) ->
  %% According to the documentation, the controlling entity may be a
  %% pid as well. We don't generate statistics in this unlikely case.
  Acc.

-spec update_stats(Node      :: node(),
                   MaybeInfo :: error | {ok, node_info()},
                   PortStats :: [port_stat()],
                   Now       :: erlang:timestamp(),
                   Acc       :: acc()) -> acc().
%% @doc Update the statistics in the accumulator given the current and
%% previous statistics from a node's distribution port.
update_stats(Node, error, PortStats, Now,
             {State, RecvIdle, SendIdle, Stats}) ->
  %% This is the first time we see this node
  [{recv_cnt, RecvCnt}, {send_cnt, SendCnt} | _] = PortStats,
  NodeInfo = #node_info{ recv_cnt  = RecvCnt
                       , send_cnt  = SendCnt
                       , last_recv = Now
                       , last_send = Now
                       },
  NodeStats = [{recv_idle, 0}, {send_idle, 0} | PortStats],
  { orddict:store(Node, NodeInfo, State)
  , RecvIdle
  , SendIdle
  , [{Node, NodeStats} | Stats]
  };
update_stats(Node, {ok, OldInfo}, PortStats, Now,
             {State, MaxRecvIdle, MaxSendIdle, Stats}) ->
  %% We have historic data from the node to compare against
  [{recv_cnt, RecvCnt}, {send_cnt, SendCnt} | _] = PortStats,
  #node_info{ recv_cnt  = OldRecvCnt
            , send_cnt  = OldSendCnt
            , last_recv = OldLastRecv
            , last_send = OldLastSend
            } = OldInfo,
  {LastRecv, RecvIdle} = get_times(OldRecvCnt, RecvCnt, OldLastRecv, Now),
  {LastSend, SendIdle} = get_times(OldSendCnt, SendCnt, OldLastSend, Now),
  NodeInfo = #node_info{ recv_cnt  = RecvCnt
                       , send_cnt  = SendCnt
                       , last_recv = LastRecv
                       , last_send = LastSend
                       },
  NodeStats = [{recv_idle, RecvIdle}, {send_idle, SendIdle} | PortStats],
  { orddict:store(Node, NodeInfo, State)
  , max(RecvIdle, MaxRecvIdle)
  , max(SendIdle, MaxSendIdle)
  , [{Node, NodeStats} | Stats]
  }.

-spec get_times(OldValue :: V,
                NewValue :: V,
                OldT     :: erlang:timestamp(),
                Now      :: erlang:timestamp()) -> {T  :: erlang:timestamp(),
                                                    Ms :: non_neg_integer()}.
%% @doc Get the last modification time and age of a metric given its
%% current and previous value and its previous modification time.
get_times(Cnt, Cnt, OldT, Now) ->
  {OldT, timer:now_diff(Now, OldT) div 1000};
get_times(_OldCnt, _NewCnt, _OldT, Now) ->
  {Now, 0}.
