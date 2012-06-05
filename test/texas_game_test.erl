-module(texas_game_test).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("game.hrl").
-include("protocol.hrl").

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  setup(),
  game:start([{wait_players, []}, {restart, []}]),
  ?assert(is_pid(?LOOKUP_GAME(1))).

id_test() ->
  setup(),
  ?assertEqual(1, game:id()),
  ?assertEqual(2, game:id()),
  ?assertEqual(3, game:id()).

list_test() ->
  setup(),
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},
  game:start(Conf),
  ?assert(is_pid(?LOOKUP_GAME(1))),
  ?assert(is_pid(?LOOKUP_GAME(2))),
  timer:sleep(1000),

  [#notify_game{}|[#notify_game{}|[]]] = game:list().

info_test() ->
  setup(),
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = no_limit, seat_count = 5, start_delay = 3000, required = 3, timeout = 1000, max = 1},
  game:start(Conf),
  ?assert(is_pid(?LOOKUP_GAME(1))),
  #notify_game{require = R, seats = C} = game:info(?LOOKUP_GAME(1)),
  ?assertEqual(3, R),
  ?assertEqual(5, C).

game_query_test() ->
  ?assert(is_list(protocol:write(#cmd_query_game{}))).

game_info_test() ->
  ?assert(is_list(protocol:write(#notify_game{
      game = 1,
      name = <<"TEXAS_TABLE">>,
      limit = #limit{min = 10, max = 400, small = 5, big = 10},
      seats = 5,
      require = 2,
      joined = 1}
    ))).

setup() ->
  catch mnesia:transaction(fun() ->
        mnesia:foldl(fun(#tab_game_xref{process = Game}, _Acc) ->
              gen_server:call(Game, kill)
          end, [], tab_game_xref)
    end
  ),
  schema_test:init().
