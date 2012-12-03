-module(rig).
-export([start/2, dispatch/2]).

-include("openpoker.hrl").

start([Cards], Ctx) when is_list(Cards) ->
  {stop, Ctx#texas{deck = deck:new(Cards)}};
start(_, Ctx) -> {stop, Ctx}.

dispatch(_R, _Ctx) ->
  ok.
