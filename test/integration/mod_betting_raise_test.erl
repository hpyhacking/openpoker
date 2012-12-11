-module(mod_betting_raise_test).

-include("openpoker.hrl").
-include("openpoker_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

-define(BUYIN, 100).
-define(SB, 10).
-define(BB, ?SB * 2).

normal_call_test_() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
  Players = ?THREE_PLAYERS,
  sim:join_and_start_game(Players),

  %% SB 10 BB 20
  B_SN = 1, SB_SN = 2, BB_SN = 3, 
  sim:check_blind(Players, B_SN, SB_SN, BB_SN),

  %% 小盲10 大盲20
  %% 开局 小盲大盲之后该庄家说话，庄家需要跟大盲才可以继续
  %% 庄家选择跟注 跟注20 加注0
  sim:check_actor(B_SN, {20, 20, 80}, Players),
  sim:check_raise(B_SN, call, {20, 0}, Players),

  %% 庄家下注后轮到小盲说话
  %% 此时庄家已经跟注20，由于之前小盲注已经下注10
  %% 这次只需要补齐10即可跟注，如果加注最少需要加20
  sim:check_actor(SB_SN, {10, 20, 80}, Players),
  sim:check_raise(SB_SN, call, {10, 0}, Players),

  %% 小盲下注后轮到大盲说话
  %% 此时小盲已经补齐跟注20，由于之前大盲注已经下注20
  %% 这次不需要跟注，可以选择看牌或者加注，加注最小要等于大盲
  sim:check_actor(BB_SN, {0, 20, 80}, Players),
  sim:check_raise(BB_SN, call, {0, 0}, Players),

  %% 所有玩家均已下相同数额的筹码，讲进入下一轮
  sim:check_notify_stage_end(?GS_PREFLOP, Players),
  sim:check_notify_stage(?GS_FLOP, Players),

  ?assertEqual(ok, ok)
end}.

normal_raise_test_() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
  Players = ?THREE_PLAYERS,
  sim:join_and_start_game(Players),

  %% SB 10 BB 20
  B_SN = 1, SB_SN = 2, BB_SN = 3, 
  sim:check_blind(Players, B_SN, SB_SN, BB_SN),

  %% b = s = d = 100
  %% d -> raise to 40
  %% s -> call
  %% b -> call

  sim:check_actor(B_SN, {20, 20, 80}, Players),
  sim:check_raise(B_SN, 20, {20, 20}, Players),

  sim:check_actor(SB_SN, {30, 40, 60}, Players),
  sim:check_raise(SB_SN, call, {30, 0}, Players),

  sim:check_actor(BB_SN, {20, 40, 60}, Players),
  sim:check_raise(BB_SN, call, {20, 0}, Players),

  sim:check_notify_stage_end(?GS_PREFLOP, Players),
  sim:check_notify_stage(?GS_FLOP, Players),

  %% b = s = d = 60
  %% s -> check
  %% b -> check
  %% d -> raise to 40
  %% s -> call
  %% b -> allin 60
  %% d -> allin
  %% s -> allin

  sim:check_actor(SB_SN, {0, 20, 60}, Players),
  sim:check_raise(SB_SN, check, {0, 0}, Players),

  sim:check_actor(BB_SN, {0, 20, 60}, Players),
  sim:check_raise(BB_SN, check, {0, 0}, Players),

  sim:check_actor(B_SN, {0, 20, 60}, Players),
  sim:check_raise(B_SN, 40, {0, 40}, Players),

  sim:check_actor(SB_SN, {40, 40, 20}, Players),
  sim:check_raise(SB_SN, call, {40, 0}, Players),

  sim:check_actor(BB_SN, {40, 40, 20}, Players),
  sim:check_raise(BB_SN, 20, {40, 20}, Players),

  sim:check_actor(B_SN, {20, 60, 0}, Players),
  sim:check_raise(B_SN, call, {20, 0}, Players),

  sim:check_actor(SB_SN, {20, 60, 0}, Players),
  sim:check_raise(SB_SN, call, {20, 0}, Players),

  sim:check_notify_stage_end(?GS_FLOP, Players),
  ?assertMatch(ok, ok)
end}.

setup_normal() ->
  setup([{op_mod_blinds, []}, {op_mod_betting, [?GS_PREFLOP]}, {op_mod_betting, [?GS_FLOP]}]).

setup(MixinMods) ->
  sim:setup(),
  sim:setup_game(
    #tab_game_config{
      module = game, seat_count = 9, required = 2,
      limit = #limit{min = 100, max = 400, small = ?SB, big = ?BB},
      mods = [{op_mod_suspend, []}, {wait_players, []}] ++ MixinMods ++ [{stop, []}],
      start_delay = 0, timeout = 1000, max = 1}).
