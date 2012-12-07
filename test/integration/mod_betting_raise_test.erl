-module(mod_betting_raise_test).

-include("openpoker.hrl").
-include("openpoker_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

-define(BUYIN, 100).
-define(SB, 10).
-define(BB, ?SB * 2).

normal_call_test() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
  Players = ?THREE_PLAYERS,
  sim:join_and_start_game(Players),

  %% SB 10 BB 20
  B_SN = 1, SB_SN = 2, BB_SN = 3, 
  sim:check_blind(Players, B_SN, SB_SN, BB_SN),

  %% 小盲10 大盲20
  %% 开局 小盲大盲之后该庄家说话，庄家需要跟大盲才可以继续
  %% 庄家选择跟注 跟注20 加注0
  sim:check_notify_actor(B_SN, Players),
  ?assertMatch(#notify_betting{call = ?BB, min = ?BB, max = ?BUYIN - ?BB}, h(?JACK)),
  sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
  sim:check_notify_raise(?BB, 0, Players),

  %% 庄家下注后轮到小盲说话
  %% 此时庄家已经跟注20，由于之前小盲注已经下注10
  %% 这次只需要补齐10即可跟注，如果加注最少需要加20
  sim:check_notify_actor(SB_SN, Players),
  ?assertMatch(#notify_betting{call = 10, min = 20, max = 80}, h(?TOMMY)),
  sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
  sim:check_notify_raise(10, 0, Players),

  %% 小盲下注后轮到大盲说话
  %% 此时小盲已经补齐跟注20，由于之前大盲注已经下注20
  %% 这次不需要跟注，可以选择看牌或者加注，加注最小要等于大盲
  sim:check_notify_actor(BB_SN, Players),
  ?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, h(?FOO)),
  sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 0}),
  sim:check_notify_raise(0, 0, Players),

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

  %% 小盲10 大盲20
  %% 开局 小盲大盲之后该庄家说话，庄家需要跟大盲才可以继续
  %% 庄家选择加注（需要加注大盲的一倍以上）跟注20 加注20
  sim:check_notify_actor(B_SN, Players),
  ?assertMatch(#notify_betting{call = ?BB, min = ?BB, max = ?BUYIN - ?BB}, h(?JACK)),
  sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = ?BB}),
  sim:check_notify_raise(?BB, ?BB, Players),

  %% 庄家下注后轮到小盲说话
  %% 此时庄家已经加注到40，由于之前小盲注已经下注10
  %% 这次只需要补齐30即可跟注，如果加注最少需要加40
  sim:check_notify_actor(SB_SN, Players),
  %?assertMatch(#notify_betting{call = 30, min = 40, max = 60}, h(?TOMMY)),
  %sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
  %sim:check_notify_raise(10, 0, Players),

        %%% BB RAISE 20
        %sim:check_notify_actor(BB, Players),
        %?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, sim_client:head(?FOO)),
        %sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 20}),
        %sim:check_notify_raise(0, 20, Players),

        %%% B CALL 20
        %sim:check_notify_actor(B, Players),
        %?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?JACK)),
        %sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        %sim:check_notify_raise(20, 0, Players),

        %%% SB CALL 20
        %sim:check_notify_actor(SB, Players),
        %?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?TOMMY)),
        %sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        %sim:check_notify_raise(20, 0, Players),

        %%% TURNOVER STAGE
        %sim:check_notify_stage_end(?GS_PREFLOP, Players),
        %sim:check_notify_stage(?GS_FLOP, Players),

        %%% SB CHECK
        %sim:check_notify_actor(SB, Players),
        %?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?TOMMY)),
        %sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        %sim:check_notify_raise(0, 0, Players),

        %%% BB RAISE 20
        %sim:check_notify_actor(BB, Players),
        %?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?FOO)),
        %sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 20}),
        %sim:check_notify_raise(0, 20, Players),

        %%% B CALL 20
        %sim:check_notify_actor(B, Players),
        %?assertMatch(#notify_betting{call = 20, min = 20, max = 40}, sim_client:head(?JACK)),
        %sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        %sim:check_notify_raise(20, 0, Players),

        %%% SB CALL
        %sim:check_notify_actor(SB, Players),
        %?assertMatch(#notify_betting{call = 20, min = 20, max = 40}, sim_client:head(?TOMMY)),
        %sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        %sim:check_notify_raise(20, 0, Players),

        %%% FLOP OVER
        %sim:check_notify_stage_end(?GS_FLOP, Players),
        %?assertMatch(stop, sim:game_state())
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

h(ID) ->
  sim_client:head(ID).
