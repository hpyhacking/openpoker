-module(game_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(MOD, [{wait_players, []}]).

config_0_test_() -> {setup, fun setup/0, fun () ->
        ?assertMatch([], game:config())
    end}.

config_1_test_() -> {setup, fun setup/0, fun () ->
        mnesia:dirty_write(#tab_game_config{id = 1, module = game, mods = ?MOD, limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 1}),
        ?assertMatch([{game, _, ?MOD}], game:config())
    end}.

config_2_test_() -> {setup, fun setup/0, fun () ->
        mnesia:dirty_write(#tab_game_config{id = 1, module = game, mods = ?MOD, limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 3}),
        ?assertMatch([{game, _, ?MOD}, {game, _, ?MOD}, {game, _, ?MOD}], game:config())
    end}.

config_3_test_() -> {setup, fun setup/0, fun () ->
        mnesia:dirty_write(#tab_game_config{id = 1, module = game, mods = ?MOD, limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 3}),
        mnesia:dirty_write(#tab_game_config{id = 2, module = fuck, mods = ?MOD, limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2}),
        ?assertMatch([{game, _, ?MOD}, {game, _, ?MOD}, {game, _, ?MOD}, {fuck, _, ?MOD}, {fuck, _, ?MOD}], game:config())
    end}.

setup() ->
  schema_test:init().
