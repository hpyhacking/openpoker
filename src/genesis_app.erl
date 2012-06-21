-module(genesis_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(genesis).

start(_StartType, _StartArgs) ->
    genesis_sup:start_link().

stop(_State) ->
    ok.
