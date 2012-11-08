-module(texas_game_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

start_test_() -> {setup, fun setup/0, fun sim:clean/1, fun () ->
        ?assert(is_pid(?LOOKUP_GAME(1))),
        ?assert(is_pid(?LOOKUP_GAME(2)))
    end}.

list_test_() -> {setup, fun setup/0, fun sim:clean/1, fun () ->
        lists:map(fun (R) -> ?assertMatch(#notify_game{}, R) end, game:list())
    end}.

info_test_() -> {setup, fun setup/0, fun sim:clean/1, fun() ->
        ?_assertMatch(#notify_game{game = 1, require = 2, seats = 9}, game:info(?GAME_NAME(1))),
        ?_assertMatch(#notify_game{game = 2, require = 2, seats = 9}, game:info(?GAME_NAME(2)))
    end}.

setup() ->
  sim:setup(),
  sim:setup_game(
    #tab_game_config{
      module = game, mods = [{wait_players, []}], limit = no_limit, 
      seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2}).
