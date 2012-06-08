-module(sim_blinds).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(GAME_CTX, game:ctx(1)).
-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

blind_headsup_game_test_() -> {setup, fun setup_blind/0, fun cleanup/1, fun () ->
        join_and_start_game(?TWO_PLAYERS),
        check_blind(?TWO_PLAYERS, 1, 1, 2),

        Ctx = ?GAME_CTX, 
        Seats = Ctx#texas.seats,
        ?assertMatch(#texas{b = #seat{sn = 1}, sb = #seat{sn = 1}, bb = #seat{sn = 2}, headsup = true}, Ctx),
        ?assertMatch(#seat{sn = 1, bet = 5, inplay = 495}, seat:get(1, Seats)),
        ?assertMatch(#seat{sn = 2, bet = 10, inplay = 490}, seat:get(2, Seats)),
        ?assertMatch([#tab_inplay{inplay = 495}], mnesia:dirty_read(tab_inplay, ?JACK_ID)),
        ?assertMatch([#tab_inplay{inplay = 490}], mnesia:dirty_read(tab_inplay, ?TOMMY_ID)),
        ?assertMatch([#tab_player_info{cash = -500}], mnesia:dirty_read(tab_player_info, ?JACK_ID)),
        ?assertMatch([#tab_player_info{cash = -500}], mnesia:dirty_read(tab_player_info, ?TOMMY_ID)),

        sim_client:send(?JACK, #cmd_leave{game = ?GAME}),
        sim_client:send(?TOMMY, #cmd_leave{game = ?GAME}),

        ?assertMatch([#tab_player_info{cash = -5}], mnesia:dirty_read(tab_player_info, ?JACK_ID)),
        ?assertMatch([#tab_player_info{cash = -10}], mnesia:dirty_read(tab_player_info, ?TOMMY_ID))
    end}.
        
blind_game_test_() -> {setup, fun setup_blind/0, fun cleanup/1, fun () ->
        join_and_start_game(?THREE_PLAYERS),
        check_blind(?THREE_PLAYERS, 1, 2, 3),

        ?assertMatch(#texas{
            b = #seat{sn = 1, pid = ?JACK_ID}, 
            sb = #seat{sn = 2, pid = ?TOMMY_ID}, 
            bb = #seat{sn = 3, pid = ?FOO_ID}, 
            headsup = false}, ?GAME_CTX)
    end}.

join_empty_game_test_() -> {setup, fun setup/0, fun cleanup/1, fun () ->
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

setup_blind() ->
  setup([{blinds, []}]).

setup() ->
  setup([]).

setup(MixinMods) ->
  schema_test:init(),
  sim_client:setup_players(?PLAYERS),

  Mods = [{wait_players, []}] ++ MixinMods ++ [{stop, []}],
  Limit = #limit{min = 100, max = 500, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = Mods, limit = Limit, seat_count = 9, start_delay = 1000, required = 2, timeout = 1000, max = 1},
  game:start(Conf).

cleanup(Games) ->
  lists:foreach(fun ({ok, Pid}) -> exch:stop(Pid) end, Games),
  lists:foreach(fun ({Key, _R}) -> sim_client:stop(Key) end, ?PLAYERS).

join_and_start_game(Players) ->
  ok = join_and_start_game(Players, 1),
  ?SLEEP,
  Len = length(Players) - 1,
  [H|_] = lists:reverse(Players),
  check_notify_join(lists:delete(H, Players), Len, Len),
  timer:sleep(1000),
  check_notify_start(Players).

join_and_start_game([], _SN) -> ok;
join_and_start_game([{Key, Id}|T], SN) ->
  sim_client:send(Key, #cmd_join{game = ?GAME, sn = SN, buyin = 500}),
  ?assertMatch(#notify_game_detail{}, sim_client:head(Key)),
  ?assertMatch(#notify_join{player = Id}, sim_client:head(Key)),
  join_and_start_game(T, SN + 1).

check_blind([], _, _, _) -> ok;
check_blind([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  check_blind(T, B, SB, BB).

check_notify_start([]) -> ok;
check_notify_start([{Key, _Id}|T]) ->
  ?assertMatch(#notify_game_start{}, sim_client:head(Key)),
  check_notify_start(T).

check_notify_join([], 0, 0) -> ok;
check_notify_join([_|T], 0, S) ->
  check_notify_join(T, S - 1, S - 1);
check_notify_join(Players = [{Key, _Id}|_], N, S) ->
  ?assertMatch(#notify_join{}, sim_client:head(Key)),
  check_notify_join(Players, N - 1, S).

