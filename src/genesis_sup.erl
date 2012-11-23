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
  Event = {op_exch_event, {op_exch_event, start_link, []}, permanent, 2000, worker, [op_exch_event]},
  GamesSup = {genesis_games_sup, {genesis_games_sup, start_link, []}, permanent, 10000, supervisor, [genesis_games_sup]},
  PlayersSup = {genesis_players_sup, {genesis_players_sup, start_link, []}, permanent, 2000, supervisor, [genesis_players_sup]},
  {ok, {{one_for_one, 5, 10}, [GamesSup, PlayersSup, Event]}}.
