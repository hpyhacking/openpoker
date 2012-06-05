-module(stop).
-export([start/2, stop/2]).

-include("common.hrl").
-include("protocol.hrl").
-include("game.hrl").

start([], Ctx) ->
  erlang:start_timer(10 * 1000, self(), ?MODULE),
  {next, stop, Ctx}.

stop({timeout, _, ?MODULE}, Ctx = #texas{}) ->
  {stop, Ctx};

stop(_, Ctx) ->
  {skip, Ctx}.
