-module(sim_login_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

connection_timeout_test() ->
  sim_client:start(?MODULE, 2000),
  ?assert(is_pid(whereis(?MODULE))),
  timer:sleep(3000),
  ?assertNot(is_pid(whereis(?MODULE))),
  ?assertMatch([#notify_error{error = ?ERR_CONNECTION_TIMEOUT}], sim_client:box()).

login_error_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, [
      fun () ->
          sim_client:start(?MODULE),
          ?assert(is_pid(whereis(?MODULE))),
          sim_client:send(?MODULE, #cmd_login{identity = <<"player">>, password = <<"def_pwd">>}),
          ?assertNot(is_pid(whereis(?MODULE))),
          ?assertMatch([#notify_error{error = ?ERR_UNAUTH}], sim_client:box())
      end,

      fun () ->
          sim_client:start(?MODULE),
          ?assert(is_pid(whereis(?MODULE))),

          Jack = sim_client:player(?JACK, ?PLAYERS),
          mnesia:dirty_write(Jack#tab_player_info{password = <<"test">>}),

          sim_client:send(?MODULE, #cmd_login{identity = <<"jack">>, password = <<"def_pwd">>}),
          ?assertNot(is_pid(whereis(?MODULE))),
          ?assertMatch([#notify_error{error = ?ERR_UNAUTH}], sim_client:box())
      end,

      fun () ->
          sim_client:start(?MODULE),
          ?assert(is_pid(whereis(?MODULE))),

          Jack = sim_client:player(?JACK, ?PLAYERS),
          mnesia:dirty_write(Jack#tab_player_info{disabled = true}),
          sim_client:send(?MODULE, #cmd_login{identity = <<"jack">>, password = <<"def_pwd">>}),
          ?assertNot(is_pid(whereis(?MODULE))),
          ?assertMatch([#notify_error{error = ?ERR_PLAYER_DISABLE}], sim_client:box())
      end]}.

login_successful_test() -> {setup, fun sim:setup/0, fun sim:clean/1, [
      fun () ->

  sim_client:start(?JACK),
  ?assert(is_pid(whereis(?JACK))),
  Jack = sim_client:player(?JACK, ?PLAYERS),
  mnesia:dirty_write(Jack),
  sim_client:send(?JACK, #cmd_login{identity = <<"jack">>, password = <<"def_pwd">>}),
  ?assert(is_pid(?LOOKUP_PLAYER(Jack#tab_player_info.pid))),
  ?assertMatch(#notify_player{}, sim_client:head(?JACK)),
  ?assertMatch(#notify_acount{}, sim_client:head(?JACK)),
  sim_client:stop(?JACK)
end]}.
