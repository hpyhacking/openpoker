-module(genesis_players_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, terminate_child/1, start_anyone/0]).

%% Supervisor callbacks
-export([init/1]).

-include("genesis_schema.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(PID(Id), {player, Id}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(R) ->
  supervisor:start_child(?MODULE, {?PID(R#tab_player_info.pid), {player, start_link, [R]}, permanent, 2000, worker, []}).

terminate_child(Id) ->
  supervisor:terminate_child(?MODULE, ?PID(Id)),
  supervisor:delete_child(?MODULE, ?PID(Id)).

start_anyone() ->
  [R] = mnesia:dirty_read(tab_player_info, 1),
  start_child(R).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.
