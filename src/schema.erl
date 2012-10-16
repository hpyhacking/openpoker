-module(schema).
-export([rebuild_schema/0, rebuild_core/0, rebuild_core_and_data/0]).

-include("genesis_game.hrl").
-include("genesis_common.hrl").
-include("genesis_schema.hrl").

-define(RAM, {ram_copies, Nodes}).
-define(DISC, {disc_copies, Nodes}).
-define(TABLE_DEF(Name, Type, Copies, Fields), {Name, [Copies, {type, Type}, {attributes, Fields}]}).

rebuild_schema() ->
  stopped = mnesia:stop(),
  ok = mnesia:delete_schema(all_nodes()),
  timer:sleep(500),
  ok = mnesia:create_schema(all_nodes()),
  timer:sleep(500),
  ok = mnesia:start().

rebuild_core() ->
  rebuild_core(all_nodes()).

rebuild_core_and_data() ->
  rebuild_core(),
  setup_games(),
  setup_players().

%% Private

rebuild_core(Nodes) ->
  rebuild_core_table(Nodes),
  setup_counters().

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
  Limit = #limit{small = 10, big = 20, min = 500, max = 1000},
  mnesia:dirty_write(#tab_game_config{id = 1, module = game, mods = ?DEF_MOD, limit = Limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 10}),
  mnesia:dirty_write(#tab_game_config{id = 2, module = game, mods = ?DEF_MOD, limit = Limit#limit{small = 100, big = 200}, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 10}),
  mnesia:dirty_write(#tab_game_config{id = 3, module = game, mods = ?DEF_MOD, limit = Limit#limit{small = 500, big = 1000}, seat_count = 5, start_delay = 3000, required = 2, timeout = 1000, max = 10}).

setup_players() ->
  mnesia:dirty_write(#tab_player_info{pid = 1, identity = "player", password = ?DEF_HASH_PWD, nick = "nick", photo = "default", cash = 10000}),
  mnesia:dirty_write(#tab_player_info{pid = 2, identity = "robot", password = ?DEF_HASH_PWD, nick = "robot", photo = "default", cash = 10000}),
  mnesia:dirty_write(#tab_player_info{pid = 3, identity = "robot_foo", password = ?DEF_HASH_PWD, nick = "robot foo", photo = "default", cash = 10000}),
  mnesia:dirty_write(#tab_player_info{pid = 4, identity = "robot_doo", password = ?DEF_HASH_PWD, nick = "robot doo", photo = "default", cash = 10000}).
