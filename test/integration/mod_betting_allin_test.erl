-module(mod_betting_allin_test).

-include("openpoker.hrl").
-include("openpoker_test.hrl").

-define(PP, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}, {?FOO, ?FOO_ID}]).
-define(B_SN, 1).
-define(SB_SN, 2).
-define(BB_SN, 3).

normal_allin_test_() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
  sim:check_actor(?B_SN, {20, 20, 0}, ?PP),
  sim:check_raise(?B_SN, call, {20, 0}, ?PP),

  sim:check_actor(?SB_SN, {10, 20, 60}, ?PP),
  sim:check_raise(?SB_SN, call, {10, 0}, ?PP),

  sim:check_actor(?BB_SN, {0, 20, 40}, ?PP),
  sim:check_raise(?BB_SN, 20, {0, 20}, ?PP),

  sim:check_actor(?SB_SN, {20, 40, 40}, ?PP),

  ?assertEqual(ok, ok)
end}.

setup_normal() ->
  setup([{op_mod_blinds, []}, {op_mod_betting, [?GS_PREFLOP]}, {op_mod_betting, [?GS_FLOP]}], 10, 20),
  sim:join_and_start_game(?PP, [20, 80, 60]),
  sim:check_blind(?PP, ?B_SN, ?SB_SN, ?BB_SN).

setup(MixinMods, SB, BB) ->
  sim:setup(),
  sim:setup_game(
    #tab_game_config{
      module = game, seat_count = 9, required = 2,
      limit = #limit{min = 0, max = 1000000, small = SB, big = BB},
      mods = [{op_mod_suspend, []}, {wait_players, []}] ++ MixinMods ++ [{stop, []}],
      start_delay = 0, timeout = 1000, max = 1}).
