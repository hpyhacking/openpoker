-module(restart).
-export([start/2]).

start([], Ctx) ->
  {goto, top, Ctx}.
