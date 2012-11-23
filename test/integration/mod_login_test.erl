-module(mod_login_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

login_timeout_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, fun() ->
        sim_client:start(?MODULE, 100),
        ?assert(is_pid(whereis(?MODULE))),
        ?SLEEP,
        ?assertNot(is_pid(whereis(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_CONNECTION_TIMEOUT}], sim_client:box())
  end}.

login_unauth_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, fun() ->
        sim_client:start(?MODULE),
        ?assert(is_pid(whereis(?MODULE))),
        sim_client:send(?MODULE, #cmd_login{identity = <<"player">>, password = <<"def_pwd">>}),
        ?assertNot(is_pid(whereis(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_UNAUTH}], sim_client:box())
    end}.

login_unauth_pwd_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, fun() ->
        sim_client:start(?MODULE),
        ?assert(is_pid(whereis(?MODULE))),

        Jack = sim_client:player(?JACK, ?PLAYERS),
        mnesia:dirty_write(Jack#tab_player_info{password = <<"test">>}),

        sim_client:send(?MODULE, #cmd_login{identity = <<"jack">>, password = <<"def_pwd">>}),
        ?assertNot(is_pid(whereis(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_UNAUTH}], sim_client:box())
    end}.

login_disable_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, fun() ->
        sim_client:start(?MODULE),
        ?assert(is_pid(whereis(?MODULE))),

        Jack = sim_client:player(?JACK, ?PLAYERS),
        mnesia:dirty_write(Jack#tab_player_info{disabled = true}),
        sim_client:send(?MODULE, #cmd_login{identity = <<"jack">>, password = <<"def_pwd">>}),
        ?assertNot(is_pid(whereis(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_PLAYER_DISABLE}], sim_client:box())
    end}.
