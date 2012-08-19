-module(genesis_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  case mnesia:system_info(tables) of
    [schema] ->
      io:format("==============================================~n"),
      io:format(" REBUILD CORE ...~n"),
      schema:rebuild_schema(),
      schema:rebuild_core_and_data(),
      io:format(" REBUILD CORE SUCCESSFUL~n");
    _ ->
      ok
  end,

  io:format("==============================================~n"),
  io:format(" STARTING GENESIS SERVER ...~n"),
  genesis_sup:start_link().

stop(_State) ->
  ok.
