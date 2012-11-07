-module(game_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(MOD, [{wait_players, []}]).

setup() ->
  schema_test:init().
