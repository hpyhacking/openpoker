-module(sim_client_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

start_test() ->
  P1 = sim_client:start(?MODULE),
  ?assert(erlang:is_process_alive(P1)),
  sim_client:stop(?MODULE),
  ?assertNot(erlang:is_process_alive(P1)).
