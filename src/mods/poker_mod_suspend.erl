-module(poker_mod_suspend).
-export([start/2, suspend/2]).

-include("genesis.hrl").

start([], Ctx) ->
  erlang:start_timer(10 * 1000, self(), ?MODULE),
  {next, suspend, Ctx}.

suspend({timeout, _, ?MODULE}, Ctx = #texas{}) ->
  {stop, Ctx};

suspend(go_go_go, Ctx) ->
  {stop, Ctx};

suspend(_, Ctx) ->
  {skip, Ctx}.

