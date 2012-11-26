-module(restart).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2]).

start([], Ctx) ->
  {goto, top, Ctx}.

dispatch(_R, _Ctx) ->
  ok.
