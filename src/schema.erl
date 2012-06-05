-module(schema).
-export([rebuild_schema/0, rebuild_core/0]).

-ifdef(TEST).
-export([init/0]).
-endif.

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

    %% TODO: move tab_agent tab_agent_daily to genesis_console
    ?TABLE_DEF(tab_agent, set, ?DISC, record_info(fields, tab_agent)),
    ?TABLE_DEF(tab_agent_daily, set, ?DISC, record_info(fields, tab_agent_daily)),
    ?TABLE_DEF(tab_agent_player, set, ?DISC, record_info(fields, tab_agent_player)),

    ?TABLE_DEF(tab_charge_log, bag, ?DISC, record_info(fields, tab_charge_log)),
    ?TABLE_DEF(tab_turnover_log, bag, ?DISC, record_info(fields, tab_turnover_log)),
    ?TABLE_DEF(tab_buyin_log, bag, ?DISC, record_info(fields, tab_buyin_log))
  ],

  create_tables(RamTables),
  create_tables(DiscTables),

  create_indices(tab_agent, [identity, parent]),
  create_indices(tab_player_info, [agent, identity]),
  create_indices(tab_buyin_log, [aid, pid, date]),
  create_indices(tab_turnover_log, [aid, pid, date]),
  create_indices(tab_charge_log, [aid, target, date]).

reload_core_default_data() ->
  setup_counters(),
  setup_agent(),
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
  counter:reset(agent, 10),
  ok.

setup_games() ->
  ok.

setup_agent() ->
  Root = #tab_agent{ aid = 1, identity = "root", password = erlang:md5("password"), root = true, parent = nil, cash = 100 * 10000 },
  {atomic, _} = mnesia:transaction( fun() -> mnesia:write(Root) end).

load_test_env() ->
  case mnesia:system_info(is_running) of
    yes ->
      TabLists = [tab_game_xref, tab_player, tab_agent, tab_agent_daily, tab_player_info, 
        tab_inplay, tab_game_config, tab_counter, tab_charge_log, tab_turnover_log, tab_buyin_log],
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

  
%% EUnit Test Case

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init() ->
  load_test_env().

init_test() ->
  init(),
  [] = mnesia:dirty_read(tab_inplay, 1),
  mnesia:dirty_write(#tab_inplay{pid = 1, inplay = 500}),
  init(),
  [] = mnesia:dirty_read(tab_inplay, 1),
  mnesia:dirty_write(#tab_inplay{pid = 1, inplay = 500}).

%buyin_log_test() ->
  %BuyInLog = #tab_buyin_log{
    %aid = 1,
    %gid = 1,
    %amt = 10,
    %cash = -10,
    %credit = 100
  %},

  %Id = now(),
  %mnesia:dirty_write(BuyInLog#tab_buyin_log{id = Id, pid = 1, date = date(), time = time()}),
  %mnesia:dirty_write(BuyInLog#tab_buyin_log{id = Id, pid = 2, date = date(), time = time()}),

  %?assertMatch([#tab_buyin_log{pid = 1}], mnesia:dirty_index_read(tab_buyin_log, 1, pid)),
  %?assertMatch([#tab_buyin_log{aid = 1}, #tab_buyin_log{aid = 1}], mnesia:dirty_index_read(tab_buyin_log, 1, aid)),
  %?assertMatch([#tab_buyin_log{pid = 1}, #tab_buyin_log{pid = 2}], mnesia:dirty_read(tab_buyin_log, Id)).

%tab_inplay_test() ->
  %mnesia:dirty_write(#tab_inplay{pid = 1, inplay = 500}),
  %mnesia:transaction(fun() ->
        %[Inplay] = mnesia:read(tab_inplay, 1, write),
        %mnesia:delete_object(Inplay)
    %end),
  %?assertMatch([], mnesia:dirty_read(tab_inplay, 1)).
-endif.
