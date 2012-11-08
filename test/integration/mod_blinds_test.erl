-module(mod_blinds_test).

-include("genesis.hrl").
-include("genesis_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

blind_headsup_game_test_() -> {setup, fun setup_blind/0, fun sim:clean/1, fun () ->
        sim:join_and_start_game(?TWO_PLAYERS),
        sim:check_blind_only_seat(?TWO_PLAYERS, 1, 1, 2),

        Ctx = sim:game_ctx(),
        Seats = Ctx#texas.seats,
        ?assertMatch(#texas{b = #seat{sn = 1}, sb = #seat{sn = 1}, bb = #seat{sn = 2}, headsup = true}, Ctx),
        ?assertMatch(#seat{sn = 1, bet = 5, inplay = 95}, seat:get(1, Seats)),
        ?assertMatch(#seat{sn = 2, bet = 10, inplay = 90}, seat:get(2, Seats)),
        ?assertMatch([#tab_inplay{inplay = 95}], mnesia:dirty_read(tab_inplay, ?JACK_ID)),
        ?assertMatch([#tab_inplay{inplay = 90}], mnesia:dirty_read(tab_inplay, ?TOMMY_ID)),
        ?assertMatch([#tab_player_info{cash = -100}], mnesia:dirty_read(tab_player_info, ?JACK_ID)),
        ?assertMatch([#tab_player_info{cash = -100}], mnesia:dirty_read(tab_player_info, ?TOMMY_ID)),

        sim_client:send(?JACK, #cmd_leave{game = ?GAME}),
        sim_client:send(?TOMMY, #cmd_leave{game = ?GAME}),

        ?assertMatch([#tab_player_info{cash = -5}], mnesia:dirty_read(tab_player_info, ?JACK_ID)),
        ?assertMatch([#tab_player_info{cash = -10}], mnesia:dirty_read(tab_player_info, ?TOMMY_ID))
    end}.
        
blind_game_test_() -> {setup, fun setup_blind/0, fun sim:clean/1, fun () ->
        sim:join_and_start_game(?THREE_PLAYERS),
        sim:check_blind_only_seat(?THREE_PLAYERS, 1, 2, 3),

        ?assertMatch(#texas{
            b = #seat{sn = 1, pid = ?JACK_ID}, 
            sb = #seat{sn = 2, pid = ?TOMMY_ID}, 
            bb = #seat{sn = 3, pid = ?FOO_ID}, 
            headsup = false}, sim:game_ctx())
    end}.

auto_compute_seat_sn_test_() -> {setup, fun setup_empty/0, fun sim:clean/1, fun () ->
        sim:join_and_start_game(?TWO_PLAYERS, ?UNDEF),
        ?assertMatch(#notify_join{player = ?JACK_ID, sn = 1}, sim_client:head(?JACK)),
        ?assertMatch(#notify_watch{player = ?TOMMY_ID}, sim_client:head(?JACK)),
        ?assertMatch(#notify_join{player = ?TOMMY_ID, sn = 2}, sim_client:head(?JACK)),
        ?assertMatch(#notify_join{player = ?TOMMY_ID, sn = 2}, sim_client:head(?TOMMY))
    end}.

%%%
%%% setup
%%%

setup_blind() ->
  setup([{blinds, []}]).

setup_empty() ->
  setup([]).

setup(MixinMods) ->
  sim:setup(),
  genesis_games_sup:start_child(
    #tab_game_config{
      module = game, seat_count = 9, required = 2,
      limit = #limit{min = 100, max = 500, small = 5, big = 10},
      mods = [{poker_mod_suspend, []}, {wait_players, []}] ++ MixinMods ++ [{stop, []}],
      start_delay = 0, timeout = 1000, max = 1}).
