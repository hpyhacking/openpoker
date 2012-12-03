-module(op_mod_suspend).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2, suspend/2]).

-include("openpoker.hrl").

start([], Ctx) ->
  erlang:start_timer(10 * 1000, self(), ?MODULE),
  {next, suspend, Ctx}.

dispatch(_R, _Ctx) ->
  ok.

suspend({timeout, _, ?MODULE}, Ctx = #texas{}) ->
  {stop, Ctx};

suspend(go_go_go, Ctx) ->
  {stop, Ctx};

suspend(_, Ctx) ->
  {skip, Ctx}.

