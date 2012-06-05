-module(schema).
-export([rebuild_schema/0, rebuild_core/0]).

-include("schema.hrl").
-include("common.hrl").

-define(RAM, {ram_copies, Nodes}).
-define(DISC, {disc_copies, Nodes}).
-define(TABLE_DEF(Name, Type, Copies, Fields), {Name, [Copies, {type, Type}, {attributes, Fields}]}).

rebuild_schema() ->
  pong = net_adm:ping('genesis_console@air'),
  stopped = mnesia:stop(),
  ok = mnesia:delete_schema(all_nodes()),
  timer:sleep(500),
  ok = mnesia:create_schema(all_nodes()),
  timer:sleep(500).

rebuild_core() ->
  rebuild_core(all_nodes()).

%% Private

rebuild_core(Nodes) ->
  rebuild_core_table(Nodes),
  reload_core_default_data().

rebuild_core_table(Nodes) ->
  RamTables = [
    ?TABLE_DEF(tab_game_xref, set, ?RAM, record_info(fields, tab_game_xref)),
    ?TABLE_DEF(tab_player, set, ?RAM, record_info(fields, tab_player))
  ],
  DiscTables = [
    ?TABLE_DEF(tab_player_info, set, ?DISC, record_info(fields, tab_player_info)),
    ?TABLE_DEF(tab_inplay, set, ?DISC, record_info(fields, tab_inplay)),
    ?TABLE_DEF(tab_game_config, set, ?DISC, record_info(fields, tab_game_config)),
    ?TABLE_DEF(tab_counter, set, ?DISC, record_info(fields, tab_counter)),

    ?TABLE_DEF(tab_turnover_log, bag, ?DISC, record_info(fields, tab_turnover_log)),
    ?TABLE_DEF(tab_buyin_log, bag, ?DISC, record_info(fields, tab_buyin_log))
  ],

  create_tables(RamTables),
  create_tables(DiscTables),

  create_indices(tab_player_info, [identity]),
  create_indices(tab_buyin_log, [pid, date]),
  create_indices(tab_turnover_log, [pid, date]).

reload_core_default_data() ->
  setup_counters(),
  setup_games().

all_nodes() ->
  nodes() ++ [node()].

create_tables([]) -> ok;
create_tables([{Name, TabDef}|T]) ->
  {atomic, ok} = mnesia:create_table(Name, TabDef),
  create_tables(T).

create_indices(_, []) -> ok;
create_indices(Name, Index) when is_atom(Index) ->
  create_indices(Name, [Index]);
create_indices(Name, [Index|T]) ->
  {atomic, ok} = mnesia:add_table_index(Name, Index),
  create_indices(Name, T).

setup_counters()->
  counter:reset(game),
  counter:reset(player),
  counter:reset(inplay_xref),
  ok.

setup_games() ->
  ok.

load_test_env() ->
  case mnesia:system_info(is_running) of
    yes ->
      TabLists = [tab_game_xref, tab_player, tab_player_info, 
        tab_inplay, tab_game_config, tab_counter, tab_turnover_log, tab_buyin_log],
      lists:map(fun(Table) -> {atomic, ok} = mnesia:clear_table(Table) end, TabLists);
    no ->
      Node = [node()],
      mnesia:delete_schema(Node),
      mnesia:create_schema(Node),

      timer:sleep(100),

      mnesia:start(),
      rebuild_core_table(Node),
      reload_core_default_data()
  end.
