-module(mod_betting_raise_test).

-include("openpoker.hrl").
-include("openpoker_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

normal_raise_test_() -> {setup, fun setup_normal/0, fun sim:clean/1, fun () ->
        Players = ?THREE_PLAYERS,
        sim:join_and_start_game(Players),

        %% SB 10 BB 20
        B = 1, SB = 2, BB = 3, 
        sim:check_blind(Players, B, SB, BB),

        %% B CALL 20
        sim:check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 80}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 20}),
        sim:check_notify_raise(20, 20, Players),

        % SB CALL 10
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 30, min = 40, max = 60}, sim_client:head(?TOMMY)),
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
  setup([{blinds, []}, {op_mod_betting, [?GS_PREFLOP]}, {op_mod_betting, [?GS_FLOP]}]).

setup(MixinMods) ->
  sim:setup(),
  sim:setup_game(
    #tab_game_config{
      module = game, seat_count = 9, required = 2,
      limit = #limit{min = 100, max = 400, small = 10, big = 20},
      mods = [{op_mod_suspend, []}, {wait_players, []}] ++ MixinMods ++ [{stop, []}],
      start_delay = 0, timeout = 1000, max = 1}).

set_sn([], Players) -> Players;
set_sn([{Key, SN}|T], Players) ->
  lists:keyreplace(Key, 1, Players, {Key, SN}),
  set_sn(T, Players).
