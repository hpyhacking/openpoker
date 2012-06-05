-module(sim_login).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLEEP, timer:sleep(500)).
-define(SLEEP(T), timer:sleep(T * 1000)).

connection_timeout_test() ->
  run_by_def(fun(_) ->
        ?assert(is_pid(sim_client:where(?MODULE))),
        ?SLEEP(3),
        ?assertNot(is_pid(sim_client:where(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_CONNECTION_TIMEOUT}], sim_client:box())
    end).

login_unauth_test() ->
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I#tab_player_info{identity = "player_new"}),
        ?assert(is_pid(sim_client:where(?MODULE))),
        sim_client:send(?MODULE, #cmd_login{identity = <<"player">>, password = <<"def_pwd">>}),
        ?assertNot(is_pid(sim_client:where(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_UNAUTH}], sim_client:box())
    end),

  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I#tab_player_info{password = "pwd"}),
        ?assert(is_pid(sim_client:where(?MODULE))),
        sim_client:send(?MODULE, #cmd_login{identity = <<"player">>, password = <<"def_pwd">>}),
        ?assertNot(is_pid(sim_client:where(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_UNAUTH}], sim_client:box())
    end).

login_player_disbale_test() ->
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I#tab_player_info{disabled = true}),
        ?assert(is_pid(sim_client:where(?MODULE))),
        sim_client:send(?MODULE, #cmd_login{identity = <<"player">>, password = <<"def_pwd">>}),
        ?assertNot(is_pid(sim_client:where(?MODULE))),
        ?assertMatch([#notify_error{error = ?ERR_PLAYER_DISABLE}], sim_client:box())
    end).

login_successful_test() ->
  run_by_def(fun([{player, I}|_]) ->
        mnesia:dirty_write(I),
        ?assert(is_pid(sim_client:where(?MODULE))),
        sim_client:send(?MODULE, #cmd_login{identity = <<"player">>, password = <<"def_pwd">>}),
        ?assertEqual(sim_client:where_player(1), sim_client:loopdata(?MODULE, player)),
        ?assertMatch(#notify_player{nick = <<"player">>, photo = <<"default">>}, sim_client:head(?MODULE)),
        ?assertMatch(#notify_acount{balance = 0, inplay = 0}, sim_client:head(?MODULE))
    end).

%%%
%%% private
%%%

run_by_def(Fun) ->
  schema:init(),
  Data = [
    { player, #tab_player_info{
      pid = 1, 
      identity = "player", 
      nick = "player",
      photo = "default",
      password = erlang:phash2(?DEF_PWD, 1 bsl 32),
      disabled = false }}],
  sim_client:start(?MODULE),
  Fun(Data).
