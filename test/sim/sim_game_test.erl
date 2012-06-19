-module(sim_game_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).

rank_test_() -> {setup, fun setup_with_rank/0, fun cleanup/1, fun () ->
        Players = ?TWO_PLAYERS,
        sim:join_and_start_game(Players),
        sim:check_blind_only_raise(Players, 1, 1, 2),

        sim:check_deal(),
        sim:check_shared(1, Players),
        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_THREE_KIND, high1 = ?CF_THREE}, sim_client:head(?TOMMY))
    end}.

shutdown_test_() -> {setup, fun setup_with_shutdown/0, fun cleanup/1, fun () ->
        Players = ?TWO_PLAYERS,
        sim:join_and_start_game(Players),
        sim:check_blind_only_raise(Players, 1, 1, 2),
        sim:check_deal(),
        sim:check_shared(3, Players),
        ?assertMatch(#notify_hand{rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?TOMMY)),

        %% betting
        sim:check_notify_stage(?GS_PREFLOP, Players),
        sim:turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        sim:turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 70),
        sim:turnover_player_raise({?JACK, Players},  {70, 70, 10}, 0),
        sim:check_notify_stage_end(?GS_PREFLOP, Players),

        ?assertMatch(#notify_cards{player = ?TOMMY_ID }, sim_client:head(?JACK)),
        ?assertMatch(#notify_cards{player = ?JACK_ID }, sim_client:head(?TOMMY)),

        ?assertMatch(#notify_hand{player = ?TOMMY_ID, rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_hand{player = ?JACK_ID, rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_hand{player = ?TOMMY_ID, rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{player = ?JACK_ID, rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?JACK)),

        sim:check_notify_win(?JACK_ID, 40 + 140, Players),
        sim:check_notify_out(?TOMMY_ID, Players),

        ?assertMatch([#tab_inplay{inplay = 190}], mnesia:dirty_read(tab_inplay, ?JACK_ID)),
        ?assertMatch([#tab_inplay{inplay = 10}], mnesia:dirty_read(tab_inplay, ?TOMMY_ID)),
        ?assertMatch([#tab_player_info{cash = -100}], mnesia:dirty_read(tab_player_info, ?JACK_ID)),
        ?assertMatch([#tab_player_info{cash = -100}], mnesia:dirty_read(tab_player_info, ?TOMMY_ID)),

        sim:check_notify_game_end(Players),
        timer:sleep(500),
        sim:check_notify_game_cancel(Players),
        sim:check_notify_leave(?TOMMY, Players)
    end}.

setup_with_shutdown() ->
  Mods = [{blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C 4C 4S ")]}, 
    {deal_cards, [2, private]}, {deal_cards, [3, shared]}, {ranking, []},
    {betting, [?GS_PREFLOP]}, {showdown, []}, {wait_players, []}],
  setup(Mods).

setup_with_rank() ->
  setup([{blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C")]}, {deal_cards, [2, private]}, {deal_cards, [1, shared]}, {ranking, []}]).

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
%%% private
%%%


