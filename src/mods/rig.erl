-module(rig).
-export([start/2]).

-include("genesis_game.hrl").

start([Cards], Ctx) when is_list(Cards) ->
  {stop, Ctx#texas{deck = deck:new(Cards)}};
start(_, Ctx) -> {stop, Ctx}.
