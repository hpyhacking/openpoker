-module(player_test).
-include("openpoker.hrl").
-include("openpoker_test.hrl").

start_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, fun () ->
        ?assert(is_tuple(sim:player_state(?JACK_ID)))
    end}.

auth_test_() -> {setup, fun sim:setup/0, fun sim:clean/1, fun () ->
        ?assertMatch({ok, pass, #tab_player_info{identity = "jack"}}, player:auth("jack", ?DEF_PWD)),
        ?assertEqual({ok, unauth}, player:auth("unknown", ?DEF_PWD)),
        ?assertEqual({ok, unauth}, player:auth("jack", "bad_pwd")),

        Info = sim_client:player(jack, ?PLAYERS),

        ?assertEqual({ok, pass, Info}, player:auth(Info, ?DEF_PWD)),
        ?assertEqual({ok, unauth}, player:auth(Info, "bad_pwd")),
        ?assertEqual({ok, player_disable}, player:auth(Info#tab_player_info{disabled = true}, ?DEF_PWD))
  end}.
