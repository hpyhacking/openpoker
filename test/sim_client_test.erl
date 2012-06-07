-module(sim_client_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(SLEEP, timer:sleep(100)).

start_test() ->
  P1 = sim_client:start(),
  ?assert(erlang:is_process_alive(P1)),
  P2 = sim_client:start(),
  ?assert(erlang:is_process_alive(P2)),
  ?assertNot(erlang:is_process_alive(P1)).

kill_game_test() ->
  schema_test:init(),
  Limit = #limit{min = 100, max = 400, small = 5, big = 10},

  game:start(#tab_game_config{module = game, mods = [{wait_players, []}], limit = Limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2}),

  ?assert(is_pid(sim_client:where_game(1))),
  ?assert(is_pid(sim_client:where_game(2))),
  ?assertNot(is_pid(sim_client:where_game(3))),

  sim_client:kill_games(),

  ?assertNot(is_pid(sim_client:where_game(1))),
  ?assertNot(is_pid(sim_client:where_game(2))),
  ?assertNot(is_pid(sim_client:where_game(3))).

sim_robot_test() ->
  schema_test:init(),
  sim_client:start_robot(test_robot),
  sim_client:send(test_robot, #cmd_login{identity = <<"NIL">>, password = <<?DEF_PWD>>}).
