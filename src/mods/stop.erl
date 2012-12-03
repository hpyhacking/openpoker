-module(stop).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2, stop/2]).

-include("openpoker.hrl").

start([], Ctx) ->
  erlang:start_timer(10 * 1000, self(), ?MODULE),
  {next, stop, Ctx}.

dispatch(_R, _Ctx) ->
  ok.

stop({timeout, _, ?MODULE}, Ctx = #texas{}) ->
  {stop, Ctx};

stop(_, Ctx) ->
  {skip, Ctx}.
