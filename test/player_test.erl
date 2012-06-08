-module(player_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

start_test_() -> {setup, fun setup/0, fun cleanup/1, fun () ->
        PL = plist(1),
        ?assert(erlang:is_process_alive(proplists:get_value(proc, PL))),
        ?assertEqual(<<"nick-jack">>, proplists:get_value(nick, PL)),
        ?assertEqual(<<"default">>, proplists:get_value(photo, PL))
    end}.

auth_test_() -> {setup, fun setup/0, fun cleanup/1, fun () ->
        ?assertMatch({ok, pass, #tab_player_info{identity = "jack"}}, player:auth("jack", ?DEF_PWD)),
        ?assertEqual({ok, unauth}, player:auth("unknown", ?DEF_PWD)),
        ?assertEqual({ok, unauth}, player:auth("jack", "bad_pwd")),

        Info = sim_client:player(jack, ?PLAYERS),

        ?assertEqual({ok, pass, Info}, player:auth(Info, ?DEF_PWD)),
        ?assertEqual({ok, unauth}, player:auth(Info, "bad_pwd")),
        ?assertEqual({ok, player_disable}, player:auth(Info#tab_player_info{disabled = true}, ?DEF_PWD))
  end}.

setup() ->
  schema_test:init(),
  lists:foreach(fun({_, R}) -> mnesia:dirty_write(R) end, ?PLAYERS),
  lists:map(fun({_, R}) -> player:start(R) end, ?PLAYERS).

cleanup([]) -> ok;
cleanup([{ok, Pid}|T]) ->
  player:stop(Pid),
  cleanup(T).

plist(Id) ->
  gen_server:call(?PLAYER(Id), plist).
