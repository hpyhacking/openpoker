-module(sim_blinds_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

blind_headsup_game_test_() -> {setup, fun setup_blind/0, fun cleanup/1, fun () ->
        sim:join_and_start_game(?TWO_PLAYERS),
        sim:check_blind_only_seat(?TWO_PLAYERS, 1, 1, 2),

        Ctx = game:ctx(1),
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
        
blind_game_test_() -> {setup, fun setup_blind/0, fun cleanup/1, fun () ->
        sim:join_and_start_game(?THREE_PLAYERS),
        sim:check_blind_only_seat(?THREE_PLAYERS, 1, 2, 3),

        ?assertMatch(#texas{
            b = #seat{sn = 1, pid = ?JACK_ID}, 
            sb = #seat{sn = 2, pid = ?TOMMY_ID}, 
            bb = #seat{sn = 3, pid = ?FOO_ID}, 
            headsup = false}, game:ctx(1))
    end}.

join_empty_game_test_() -> {setup, fun setup_with_join_empty_game/0, fun cleanup/1, fun () ->
        sim_client:send(?TOMMY, #cmd_watch{game = ?GAME}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?TOMMY)),

        sim_client:send(?JACK, #cmd_watch{game = ?GAME}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_join{game = ?GAME, sn = 1, buyin = 500}),
        ?assertMatch(#notify_join{}, sim_client:head(?JACK)),

        ?assertMatch(#notify_join{player = ?JACK_ID}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_join{game = ?GAME, sn = 2, buyin = 500}),
        ?assertMatch(#notify_join{player = ?TOMMY_ID}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_join{player = ?TOMMY_ID}, sim_client:head(?JACK))
    end}.

auto_compute_seat_sn_test_() -> {setup, fun setup/0, fun cleanup/1, fun () ->
        sim_client:send(?JACK, #cmd_join{game = ?GAME, sn = 0, buyin = 500}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?JACK)),
        ?assertMatch(#notify_join{player = ?JACK_ID, sn = 1}, sim_client:head(?JACK)),
        sim_client:send(?TOMMY, #cmd_join{game = ?GAME, sn = 0, buyin = 500}),
        ?assertMatch(#notify_game_detail{}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_join{player = ?TOMMY_ID, sn = 2}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_join{player = ?TOMMY_ID, sn = 2}, sim_client:head(?JACK))
    end}.

sample_test_() -> {setup, fun setup/0, fun cleanup/1, fun () ->
        ?assert(is_pid(whereis(?JACK))),
        ?assert(is_pid(whereis(?TOMMY)))
    end}.

%%%
%%% setup & cleanup
%%%

setup_blind() ->
  setup([{blinds, []}], 500).

setup_with_join_empty_game() ->
  setup([], 1000).

setup() ->
  setup([], 500).

setup(MixinMods, StartDelay) ->
  schema_test:init(),
  sim_client:setup_players(?PLAYERS),

  Mods = [{wait_players, []}] ++ MixinMods ++ [{stop, []}],
  Limit = #limit{min = 100, max = 500, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = Mods, limit = Limit, seat_count = 9, start_delay = StartDelay, required = 2, timeout = 500, max = 1},
  game:start(Conf).

cleanup(Games) ->
  lists:foreach(fun ({ok, Pid}) -> exch:stop(Pid) end, Games),
  lists:foreach(fun ({Key, _R}) -> sim_client:stop(Key) end, ?PLAYERS).
