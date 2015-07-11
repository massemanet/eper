%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Collects info about mnesia and ets/dets tables.
%%%
%%% @author Matthias Nilsson <matthias@baktus.se>
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(prfMnesia).

-export([collect/1, config/2]).

-export([available_collectors/0]).

-record(state, { collectors = default_collectors(),
                 cache = [],
                 timestamp = now()
               }).

%% We settle for only measuring changes over seconds
-define(TIME_DIVISOR, 1000000).

-type bytes()       :: non_neg_integer().
-type cache()       :: [metric()].
-type config()      :: [{config_name(), value()}].
-type config_name() :: atom().
-type count()       :: non_neg_integer().
-type data()        :: {metric_name(), [metric()]}.
-type metric()      :: {metric_name(), value()}.
-type metric_gauge():: {metric_name(), metric_value()}.
-type metric_name() :: atom().
-type metric_value():: float() | undefined.
-type collector()   :: {metric_name(), fun()}.
-type state()       :: #state{}.
-type table_name()  :: dets:tab_name() | ets:tab().
-type table_type()  :: dets | ets | remote_only.
-type time_diff()   :: number().
-type value()       :: term().

%%%_* API =====================================================================

%% @doc Collect information about mnesia and ets/dets tables.
%%
%% Default collectors are held_locks, current_transactions,
%% failed_transactions, and committed_transactions.
%%
%% @end
-spec collect('init' | state()) -> {state(), data()}.
collect(init) ->
  collect(#state{});
collect(State) ->
  Data = collect_data(State),
  {new_state(State, Data), {?MODULE, Data}}.


%% @doc Configure the running state of prfMnesia.
%%
%% Available config parameters:
%%
%% [parameter]     [unit]
%% collectors      [collector*]
%%
%% * as listed by {@link available_collectors/0}
%%
%% @end
-spec config(state(), config()) -> state().
config(State, ConfigData) ->
  Collectors = case lists:keyfind(collectors, 1, ConfigData) of
            false         -> default_collectors();
            {collectors, all}  -> available_collectors();
            {collectors, List} ->
              [Collector || Collector <- List,
                       lists:member(Collector, available_collectors())]
    end,
  State#state{ collectors = Collectors }.

%% @doc List available collectors
%%
%% The following collectors are available:
%%
%% <table>
%%   <tr>
%%     <th>[collector]</th><th>[unit]</th>
%%   </tr>
%%   <tr>
%%     <td>held_locks</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>held_locks_change</td><td>count/s</td>
%%   </tr>
%%   <tr>
%%     <td>lock_queue</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>lock_queue_change</td><td>count/s</td>
%%   </tr>
%%   <tr>
%%     <td>subscribers</td><td>[pid()]</td>
%%   </tr>
%%   <tr>
%%     <td>table_sizes</td><td>[bytes]</td>
%%   </tr>
%%   <tr>
%%     <td>table_size_changes</td><td>[bytes/s]</td>
%%   </tr>
%%   <tr>
%%     <td>object_counts</td><td>[count]</td>
%%   </tr>
%%   <tr>
%%     <td>object_count_changes</td><td>[count/s]</td>
%%   </tr>
%%   <tr>
%%     <td>current_transactions</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>current_transactions_change</td><td>count/s</td>
%%   </tr>
%%   <tr>
%%     <td>failed_transactions</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>failed_transactions_change</td><td>count/s</td>
%%   </tr>
%%   <tr>
%%     <td>committed_transactions</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>committed_transactions_change</td><td>count/s</td>
%%   </tr>
%%   <tr>
%%     <td>restarted_transactions</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>restarted_transactions_change</td><td>count/s</td>
%%   </tr>
%%   <tr>
%%     <td>logged_transactions</td><td>count</td>
%%   </tr>
%%   <tr>
%%     <td>logged_transactions_change</td><td>count/s</td>
%%   </tr>
%% </table>
%%
%% @end
-spec available_collectors() -> [metric_name()].
available_collectors() ->
  get_collector_names(collectors()).


%%%_* Gathering functions =====================================================

-spec collect_data(state()) -> [metric()].
collect_data(State) ->
  CurrentTime = erlang:now(),
  Data = collect_values(State) ++ collect_changes(State, CurrentTime),
  [{timestamp, CurrentTime}|Data].

-spec collect_values(state()) -> [metric()].
collect_values(State) ->
  ToCollect = get_value_collectors(State#state.collectors),
  [{Collector, get_metric(Collector)} || Collector <- ToCollect].

-spec collect_changes(state(), erlang:timestamp()) -> [metric()].
collect_changes(State, CurrentTime) ->
  ToCollect = get_change_collectors(State#state.collectors),
  Cache     = State#state.cache,
  Then      = State#state.timestamp,
  TimeDiff  = time_diff(Then, CurrentTime),
  [{Collector,
    get_metric(Collector, Cache, TimeDiff)} || Collector <- ToCollect].

-spec get_metric(metric_name()) -> value().
get_metric(MetricName) ->
  {MetricName, CollectorFun} = lists:keyfind(MetricName, 1,
                                             counters() ++ lists()),
  CollectorFun().

-spec get_metric(metric_name(), cache(), time_diff()) -> value().
get_metric(MetricName, Cache, TimeDiff) ->
  {MetricName, CollectorFun} = lists:keyfind(MetricName, 1, changes()),
  CollectorFun(Cache, TimeDiff).


%%%_* Collector specs =========================================================

-spec default_collectors() -> [metric_name()].
default_collectors() ->
  [held_locks, current_transactions, failed_transactions,
   committed_transactions].

-spec get_value_collectors() -> [metric_name()].
get_value_collectors() ->
  get_collector_names(counters() ++ lists()).

-spec get_value_collectors([metric_name()]) -> [metric_name()].
get_value_collectors(Collectors) ->
  filter_collectors(Collectors, get_value_collectors()).

-spec get_change_collectors() -> [metric_name()].
get_change_collectors() ->
  get_collector_names(changes()).

-spec get_collector_names([collector()]) -> [metric_name()].
get_collector_names(Collectors) ->
  [Name || {Name, _CollectorFun} <- Collectors].

-spec get_change_collectors([metric_name()]) -> [metric_name()].
get_change_collectors(Collectors) ->
  filter_collectors(Collectors, get_change_collectors()).

-spec filter_collectors([metric_name()], [metric_name()]) -> [metric_name()].
filter_collectors(Collectors, AllowedCollectors) ->
  [Collector || Collector <- Collectors,
                lists:member(Collector, AllowedCollectors)].

%% There are three types of statistics: counters, lists, and change
%% since last time. The first two has a zero-arity collector function,
%% the last a two-arity collector function where the first argument is
%% the cache and the second argument is the time since last call.
-spec collectors() -> [collector()].
collectors() ->
  counters() ++ lists() ++ changes().

-spec counters() -> [{metric_name(), fun()}].
counters() ->
  [ {held_locks,                    fun held_locks/0},
    {lock_queue,                    fun lock_queue/0},
    {subscribers,                   fun subscribers/0},
    {table_sizes,                   fun table_sizes/0},
    {object_counts,                 fun object_counts/0},
    {current_transactions,          fun current_transactions/0},
    {failed_transactions,           fun failed_transactions/0},
    {committed_transactions,        fun committed_transactions/0},
    {restarted_transactions,        fun restarted_transactions/0},
    {logged_transactions,           fun logged_transactions/0}
  ].

-spec lists() -> [{metric_name(), fun()}].
lists() ->
  [ {tables,                        fun tables/0}
  ].

-spec changes() -> [{metric_name(), fun()}].
changes() ->
  [ {held_locks_change,             fun held_locks_change/2},
    {lock_queue_change,             fun lock_queue_change/2},
    {table_size_changes,            fun table_size_changes/2},
    {object_count_changes,          fun object_count_changes/2},
    {current_transactions_change,   fun current_transactions_change/2},
    {failed_transactions_change,    fun failed_transactions_change/2},
    {committed_transactions_change, fun committed_transactions_change/2},
    {restarted_transactions_change, fun restarted_transactions_change/2},
    {logged_transactions_change,    fun logged_transactions_change/2}
  ].


%%%_* Helpers =================================================================

-spec new_state(state(), [{metric_name(), value()}]) -> state().
new_state(State, Data) ->
  Cache = [D || {Tag, _} = D <- Data,
                lists:member(Tag, available_collectors())],
  {timestamp, Timestamp} = lists:keyfind(timestamp, 1, Data),
  State#state{ cache = Cache, timestamp = Timestamp }.

-spec time_diff(erlang:timestamp(), erlang:timestamp()) -> float().
time_diff(Then, Now) ->
  timer:now_diff(Now, Then) / ?TIME_DIVISOR.

-spec calculate_changes([metric()], value() | [value()], time_diff()) ->
                           [metric_gauge()].
calculate_changes(CurrentValues, OldValues, TimeDiff) when
    is_list(CurrentValues) ->
  F = fun(CurrentValue) ->
          calculate_change(CurrentValue, OldValues, TimeDiff)
      end,
  lists:map(F, CurrentValues).

-spec calculate_change(metric() | value(), value(), time_diff()) ->
                          metric_gauge().
calculate_change({Key, _CurrentValue}, undefined, _TimeDiff) ->
  {Key, undefined};
calculate_change(_CurrentValue, undefined, _TimeDiff) ->
  undefined;
calculate_change({Key, CurrentValue}, OldValues, TimeDiff) when
    is_list(OldValues) ->
  case lists:keyfind(Key, 1, OldValues) of
    {Key, OldValue} -> {Key, (CurrentValue - OldValue) / TimeDiff};
    false           -> {Key, CurrentValue}
  end;
calculate_change(CurrentValue, OldValue, TimeDiff) when
    is_number(OldValue) ->
  (CurrentValue - OldValue) / TimeDiff.

-spec get_value_from_cache(metric_name(), cache()) -> value() | undefined.
get_value_from_cache(Key, Cache) ->
  case lists:keyfind(Key, 1, Cache) of
    {Key, Value} -> Value;
    false        -> undefined
  end.


%%%_* mnesia stats ============================================================

-spec held_locks() -> count().
held_locks() ->
  ets:info(mnesia_held_locks, size).

-spec held_locks_change(cache(), time_diff()) -> metric_gauge().
held_locks_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(held_locks, Cache),
  calculate_change(held_locks(), OldCount, TimeDiff).

-spec lock_queue() -> count().
lock_queue() ->
  ets:info(mnesia_lock_queue, size).

-spec lock_queue_change(cache(), time_diff()) -> metric_gauge().
lock_queue_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(lock_queue, Cache),
  calculate_change(lock_queue(), OldCount, TimeDiff).

-spec subscribers() -> [pid()].
subscribers() ->
  mnesia:system_info(subscribers).

-spec tables() -> [table_name()].
tables() ->
  mnesia:system_info(tables).

%% NB: Table size is in bytes
-spec table_sizes() -> [{table_name(), bytes()}].
table_sizes() ->
  F = fun(Table, Acc) ->
          case table_size(Table) of
            undefined -> Acc;
            Size      -> [{Table, Size}|Acc]
          end
      end,
  lists:foldl(F, [], tables()).

-spec table_size_changes(cache(), time_diff()) -> [metric_gauge()].
table_size_changes(Cache, TimeDiff) ->
  OldSizes = get_value_from_cache(table_sizes, Cache),
  calculate_changes(table_sizes(), OldSizes, TimeDiff).

-spec object_counts() -> [{table_name(), count()}].
object_counts() ->
  F = fun(Table, Acc) ->
          case object_count(Table) of
            undefined -> Acc;
            Count     -> [{Table, Count}|Acc]
          end
      end,
  lists:foldl(F, [], tables()).

-spec object_count_changes(cache(), time_diff()) -> [metric_gauge()].
object_count_changes(Cache, TimeDiff) ->
  OldCounts = get_value_from_cache(object_counts, Cache),
  calculate_changes(object_counts(), OldCounts, TimeDiff).

-spec current_transactions() -> count().
current_transactions() ->
  length(mnesia:system_info(transactions)).

-spec current_transactions_change(cache(), time_diff()) -> metric_gauge().
current_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(current_transactions, Cache),
  calculate_change(current_transactions(), OldCount, TimeDiff).

-spec failed_transactions() -> count().
failed_transactions() ->
  mnesia:system_info(transaction_failures).

-spec failed_transactions_change(cache(), time_diff()) -> metric_gauge().
failed_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(failed_transactions, Cache),
  calculate_change(failed_transactions(), OldCount, TimeDiff).

-spec committed_transactions() -> count().
committed_transactions() ->
  mnesia:system_info(transaction_commits).

-spec committed_transactions_change(cache(), time_diff()) -> metric_gauge().
committed_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(committed_transactions, Cache),
  calculate_change(committed_transactions(), OldCount, TimeDiff).

-spec restarted_transactions() -> count().
restarted_transactions() ->
  mnesia:system_info(transaction_restarts).

-spec restarted_transactions_change(cache(), time_diff()) -> metric_gauge().
restarted_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(restarted_transactions, Cache),
  calculate_change(restarted_transactions(), OldCount, TimeDiff).

-spec logged_transactions() -> count().
logged_transactions() ->
  mnesia:system_info(transaction_log_writes).

-spec logged_transactions_change(cache(), time_diff()) -> metric_gauge().
logged_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(logged_transactions, Cache),
  calculate_change(restarted_transactions(), OldCount, TimeDiff).

-spec object_count(table_name()) -> count() | undefined.
object_count(Table) ->
  object_count(Table, get_term_storage_type(Table)).

-spec object_count(table_name(), table_type()) -> count() | undefined.
object_count(_Table, remote_only) -> undefined;
object_count(Table,  dets)        -> dets_object_count(Table);
object_count(Table,  ets)         -> ets_object_count(Table).

-spec table_size(table_name()) -> bytes() | undefined.
table_size(Table) ->
  table_size(Table, get_term_storage_type(Table)).

-spec table_size(table_name(), table_type()) -> bytes() | undefined.
table_size(_Table, remote_only) -> undefined;
table_size(Table,  dets)        -> dets_size(Table);
table_size(Table,  ets)         -> ets_size(Table).

-spec get_term_storage_type(table_name()) -> table_type().
get_term_storage_type(Table) ->
  case mnesia:table_info(Table, storage_type) of
    disc_only_copies -> dets;
    undefined        -> remote_only;
    _                -> ets
  end.


%%%_* dets stats ==============================================================

-spec dets_size(dets:tab_name()) -> bytes().
dets_size(Table) ->
  dets:info(Table, file_size).

-spec dets_object_count(dets:tab_name()) -> count().
dets_object_count(Table) ->
  dets:info(Table, size).


%%%_* ets stats ===============================================================

-spec ets_size(ets:tab()) -> bytes().
ets_size(Table) ->
  ets:info(Table, memory) * erlang:system_info(wordsize).

-spec ets_object_count(ets:tab()) -> count().
ets_object_count(Table) ->
  ets:info(Table, size).


%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
