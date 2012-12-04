-module(mod_betting_headsup_test).

-include("openpoker.hrl").
-include("openpoker_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).

headsup_betting_one_call_one_raise_test_() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
        Players = ?TWO_PLAYERS,
        sim:join_and_start_game(Players),

        B = 1, SB = 1, BB = 2, 
        sim:check_blind(Players, B, SB, BB),

        %% sb在首轮先下注，sb需要补齐bb的盲注。
        %% SB is Button, PREFLOP SB first betting CALL 10
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 10, min = 20, max = 80}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}), %% CALL
        sim:check_notify_raise(10, 0, Players), %% CALL 10

        %% bb在首轮后下注，此时由于所有玩家筹码持平，可以check也可以raise
        %% BB only last betting on PREFLOP
        sim:check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 20}), %% RAISE
        sim:check_notify_raise(0, 20, Players),

        %% sb需要跟进bb的raise才能继续，当然也可继续加注。
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}), %% CALL
        sim:check_notify_raise(20, 0, Players), %% CALL 20

        %% 加注一次后所有玩家筹码持平，进入下一轮
        sim:check_notify_stage_end(?GS_PREFLOP, Players),
        sim:check_notify_stage(?GS_FLOP, Players),

        %% FLOP 由bb首先下注（headsup规则）
        sim:check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}), %% CHECK
        sim:check_notify_raise(0, 0, Players), %% CHECK

        %% sb同样check
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}), %% CHECK
        sim:check_notify_raise(0, 0, Players), %% CHECK

        sim:check_notify_stage_end(?GS_FLOP, Players)

    end}.

headsup_betting_one_call_one_check_test_() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
        Players = ?TWO_PLAYERS,
        sim:join_and_start_game(Players),

        B = 1, SB = 1, BB = 2, 
        sim:check_blind(Players, B, SB, BB),

        %% SB is Button, PREFLOP SB first betting CALL 10
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 10, min = 20, max = 80}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}), %% CALL
        sim:check_notify_raise(10, 0, Players), %% CALL 10

        %% BB only last betting on PREFLOP
        sim:check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}), %% CHECK
        sim:check_notify_raise(0, 0, Players),

        sim:check_notify_stage_end(?GS_PREFLOP, Players)
    end}.

setup_normal() ->
  setup([{blinds, []}, {op_mod_betting, [?GS_PREFLOP]}, {op_mod_betting, [?GS_FLOP]}, {stop, []}]).

setup_restart() ->
  setup([{blinds, []}, {op_mod_betting, [?GS_PREFLOP]}, {op_mod_betting, [?GS_FLOP]}, {restart, []}]).

setup(MixinMods) ->
  sim:setup(),
  sim:setup_game(
    #tab_game_config{
      module = game, seat_count = 9, required = 2,
      limit = #limit{min = 100, max = 400, small = 10, big = 20},
      mods = [{op_mod_suspend, []}, {wait_players, []}] ++ MixinMods,
      start_delay = 0, timeout = 1000, max = 1}).
