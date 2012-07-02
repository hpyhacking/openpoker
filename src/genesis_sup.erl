-module(genesis_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Child = case init:get_argument(protocoltest) of
    {ok, _} ->
      [{protocol_srv, 
          {websocket_server, start_link, [protocol_srv, "127.0.0.1", 8000, nil]},
          transient, 20000, worker, [websocket_server]}];
    error ->
      []
  end,
  {ok, { {one_for_one, 5, 10}, Child} }.
