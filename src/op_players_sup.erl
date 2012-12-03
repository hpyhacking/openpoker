-module(op_players_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, terminate_child_ex/1, terminate_child/1, start_anyone/0]).

%% Supervisor callbacks
-export([init/1]).

-include("openpoker.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(PID(Id), {player, Id}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(R) ->
  supervisor:start_child(?MODULE, {?PID(R#tab_player_info.pid), {player, start_link, [R]}, permanent, brutal_kill, worker, []}).

terminate_child_ex(Id) ->
  spawn(?MODULE, terminate_child, [Id]).

terminate_child(Id) ->
  ok = supervisor:terminate_child(?MODULE, ?PID(Id)),
  ok = supervisor:delete_child(?MODULE, ?PID(Id)).

start_anyone() ->
  [R] = mnesia:dirty_read(tab_player_info, 1),
  start_child(R).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.
