-module(sim_betting_test).

-include("genesis.hrl").
-include("genesis_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

normal_betting_test_() -> {setup, fun setup_normal/0, fun cleanup/1, fun () ->
        Players = ?THREE_PLAYERS,
        sim:join_and_start_game(Players),

        %% SB 10 BB 20
        B = 1, SB = 2, BB = 3, 
        sim:check_blind(Players, B, SB, BB),

        %% B CALL 20
        sim:check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 80}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(20, 0, Players),

        %% SB CALL 10
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 10, min = 20, max = 80}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(10, 0, Players),

        %% BB RAISE 20
        sim:check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, sim_client:head(?FOO)),
        sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 20}),
        sim:check_notify_raise(0, 20, Players),

        %% B CALL 20
        sim:check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(20, 0, Players),

        %% SB CALL 20
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(20, 0, Players),

        %% TURNOVER STAGE
        sim:check_notify_stage_end(?GS_PREFLOP, Players),
        sim:check_notify_stage(?GS_FLOP, Players),

        %% SB CHECK
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(0, 0, Players),

        %% BB RAISE 20
        sim:check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?FOO)),
        sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 20}),
        sim:check_notify_raise(0, 20, Players),

        %% B CALL 20
        sim:check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 40}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(20, 0, Players),

        %% SB CALL
        sim:check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 40}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        sim:check_notify_raise(20, 0, Players),

        %% FLOP OVER
        sim:check_notify_stage_end(?GS_FLOP, Players),
        ?assertMatch(stop, game:state(?GAME))
    end}.

normal_betting_and_fold_test_() -> {setup, fun setup_normal/0, fun cleanup/1, fun () ->
        B = 1, SB = 2, BB = 3,
        Players = set_sn([{?JACK, B}, {?TOMMY, SB}, {?FOO, BB}], ?THREE_PLAYERS),

        sim:join_and_start_game(Players),
        sim:check_blind(Players, B, SB, BB),

        sim:turnover_player_raise({?JACK, Players},  {20, 20, 80}, 0),
        sim:turnover_player_raise({?TOMMY, Players}, {10, 20, 80}, 0),
        sim:turnover_player_raise({?FOO, Players},   { 0, 20, 80}, 0),

        sim:check_notify_stage_end(?GS_PREFLOP, Players),
        sim:check_notify_stage(?GS_FLOP, Players),

        sim:turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 20),
        sim:turnover_player_fold ({?FOO, Players},   {20, 20, 60}),
        sim:turnover_player_raise({?JACK, Players},  {20, 20, 60}, 0),

        sim:check_notify_stage_end(?GS_FLOP, Players),
        ?assertMatch(#texas{joined = 3}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end}.

normal_betting_and_leave_test_() -> {setup, fun setup_normal/0, fun cleanup/1, fun () ->
        B = 1, SB = 2, BB = 3,
        Players = set_sn([{?JACK, B}, {?TOMMY, SB}, {?FOO, BB}], ?THREE_PLAYERS),

        sim:join_and_start_game(Players),
        sim:check_blind(Players, B, SB, BB),

        sim:turnover_player_raise({?JACK, Players},  {20, 20, 80}, 0),
        sim:turnover_player_raise({?TOMMY, Players}, {10, 20, 80}, 0),
        sim:turnover_player_raise({?FOO, Players},   { 0, 20, 80}, 0),

        sim:check_notify_stage_end(?GS_PREFLOP, Players),
        sim:check_notify_stage(?GS_FLOP, Players),

        sim:turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 20),
        sim:turnover_player_leave({?FOO, Players},   {20, 20, 60}),
        sim:turnover_player_raise({?JACK, proplists:delete(?FOO, Players)},  {20, 20, 60}, 0),

        sim:check_notify_stage_end(?GS_FLOP, proplists:delete(?FOO, Players)),
        ?assertMatch(#texas{joined = 2}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end}.

headsup_betting_test_() -> {setup, fun setup_normal/0, fun cleanup/1, fun () ->
        SB = 1, BB = 2,
        Players = set_sn([{?JACK, SB}, {?TOMMY, BB}], ?TWO_PLAYERS),

        sim:join_and_start_game(Players),
        sim:check_blind(Players, SB, SB, BB),

        sim:turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        sim:turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 0),

        sim:check_notify_stage_end(?GS_PREFLOP, Players),
        sim:check_notify_stage(?GS_FLOP, Players),

        sim:turnover_player_raise({?TOMMY, Players}, {0, 20, 80}, 0),
        sim:turnover_player_raise({?JACK, Players},  {0, 20, 80}, 20),
        sim:turnover_player_raise({?TOMMY, Players}, {20, 20, 60}, 0),

        sim:check_notify_stage_end(?GS_FLOP, Players),
        ?assertMatch(stop, game:state(?GAME))
    end}.

headsup_betting_and_fold_test_() -> {setup, fun setup_normal/0, fun cleanup/1, fun () ->
        SB = 1, BB = 2,
        Players = set_sn([{?JACK, SB}, {?TOMMY, BB}], ?TWO_PLAYERS),

        sim:join_and_start_game(Players),
        sim:check_blind(Players, SB, SB, BB),

        sim:turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        sim:turnover_player_fold({?TOMMY, Players},  { 0, 20, 80}),

        ?assertMatch(#texas{joined = 2}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end}.

headsup_betting_and_leave_test_() -> {setup, fun setup_normal/0, fun cleanup/1, fun () ->
        SB = 1, BB = 2,
        Players = set_sn([{?JACK, SB}, {?TOMMY, BB}], ?TWO_PLAYERS),

        sim:join_and_start_game(Players),
        sim:check_blind(Players, SB, SB, BB),

        sim:turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        sim:turnover_player_leave({?TOMMY, Players}, { 0, 20, 80}),


        ?assertMatch(#texas{joined = 1}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end}.

setup_normal() ->
  setup([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}]).

setup(MixinMods) ->
  schema_test:init(),
  sim_client:setup_players(?PLAYERS),

  Mods = [{wait_players, []}] ++ MixinMods ++ [{stop, []}],
  Limit = #limit{min = 100, max = 400, small = 10, big = 20},
  Conf = #tab_game_config{module = game, mods = Mods, limit = Limit, seat_count = 9, start_delay = 500, required = 2, timeout = 1000, max = 1},
  game:start(Conf).

cleanup(Games) ->
  lists:foreach(fun ({ok, Pid}) -> exch:stop(Pid) end, Games),
  lists:foreach(fun ({Key, _R}) -> sim_client:stop(Key) end, ?PLAYERS).

%%%
%%% private test until
%%%

set_sn([], Players) -> Players;
set_sn([{Key, SN}|T], Players) ->
  lists:keyreplace(Key, 1, Players, {Key, SN}),
  set_sn(T, Players).
