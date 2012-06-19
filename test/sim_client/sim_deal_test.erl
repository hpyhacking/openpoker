-module(sim_deal_test).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).

private_rank_test_() -> {setup, fun setup_with_private_rank/0, fun cleanup/1, fun () ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),

        check_blind(Players, 1, 1, 2),
        check_deal(),

        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_THREE}, sim_client:head(?TOMMY))
    end}.

deal_test_() -> {setup, fun setup_with_deal/0, fun cleanup/1, fun () ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),

        check_blind(Players, 1, 1, 2),
        check_deal(),
        check_shared(3, Players),

        Ctx = game:ctx(?GAME),
        Jack = seat:get(1, Ctx#texas.seats),
        Tommy = seat:get(2, Ctx#texas.seats),
        ?assertEqual(52 - 4 - 3, deck:size(Ctx#texas.deck)),
        ?assertEqual(3, length(Ctx#texas.board)),
        ?assertEqual(2, hand:size(Jack#seat.hand)),
        ?assertEqual(2, hand:size(Tommy#seat.hand)),
        ?assertMatch(stop, game:state(?GAME))
    end}.

share_rank_test_() -> {setup, fun setup_with_share_rank/0, fun cleanup/1, fun () ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),
        check_deal(),
        check_shared(1, Players),
        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_THREE_KIND, high1 = ?CF_THREE}, sim_client:head(?TOMMY))
    end}.

share_rank2_test_() -> {setup, fun setup_with_share_rank2/0, fun cleanup/1, fun () ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),
        check_deal(),
        check_shared(3, Players),
        ?assertMatch(#notify_hand{rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?TOMMY))
    end}.

setup_with_share_rank2() ->
  MixinMods = [{blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C 4C 4S ")]}, {deal_cards, [2, private]}, {deal_cards, [3, shared]}, {ranking, []}],
  setup(MixinMods).

setup_with_share_rank() ->
  MixinMods = [{blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C")]}, {deal_cards, [2, private]}, {deal_cards, [1, shared]}, {ranking, []}],
  setup(MixinMods).

setup_with_deal() ->
  MixinMods = [{blinds, []}, {deal_cards, [2, private]}, 
    {deal_cards, [1, shared]}, {deal_cards, [1, shared]}, {deal_cards, [1, shared]}],
  setup(MixinMods).

setup_with_private_rank() ->
  RigCards = hand:make_cards("3H 4H 3D 4D"),
  MixinMods = [{blinds, []}, {rig, [RigCards]}, {deal_cards, [2, private]}, {ranking, []}],
  setup(MixinMods).

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
%%% Private
%%%

check_shared(N, Players) ->
  check_shared(N, N, Players).

check_shared(_, _, []) -> ok;
check_shared(N, 0, [_|T]) -> 
  check_shared(N, N, T);
check_shared(N, S, L = [{Key, _Id}|_T]) ->
  ?assertMatch(#notify_shared{game = ?GAME}, sim_client:head(Key)),
  check_shared(N, S - 1, L).

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

check_blind([], _, _, _) -> ok;
check_blind([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 10}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 20}, sim_client:head(Key)),
  check_blind(T, B, SB, BB).

check_deal() ->
  %% Tommy is left button player, first deal card.
  %% Button is last deal card player, in this is Jack.

  %% tommy first card
  ?assertMatch(#notify_draw{game = ?GAME, player = ?TOMMY_ID}, sim_client:head(?JACK)),
  ?assertMatch(#notify_private{game = ?GAME, player = ?TOMMY_ID}, sim_client:head(?TOMMY)),

  %% jack first card
  ?assertMatch(#notify_draw{game = ?GAME, player = ?JACK_ID}, sim_client:head(?TOMMY)),
  ?assertMatch(#notify_private{game = ?GAME, player = ?JACK_ID}, sim_client:head(?JACK)),

  %% tommy second card
  ?assertMatch(#notify_draw{game = ?GAME, player = ?TOMMY_ID}, sim_client:head(?JACK)),
  ?assertMatch(#notify_private{game = ?GAME, player = ?TOMMY_ID}, sim_client:head(?TOMMY)),

  %% jack second card
  ?assertMatch(#notify_draw{game = ?GAME, player = ?JACK_ID}, sim_client:head(?TOMMY)),
  ?assertMatch(#notify_private{game = ?GAME, player = ?JACK_ID}, sim_client:head(?JACK)).

join_and_start_game(Players) ->
  ok = join_and_start_game(Players, 1),
  ?SLEEP,
  Len = length(Players) - 1,
  [H|_] = lists:reverse(Players),
  check_notify_join(lists:delete(H, Players), Len, Len),
  check_notify_start(Players).

join_and_start_game([], _SN) -> ok;
join_and_start_game([{Key, Id}|T], SN) ->
  sim_client:send(Key, #cmd_join{game = ?GAME, sn = SN, buyin = 100}),
  ?assertMatch(#notify_game_detail{}, sim_client:head(Key)),
  ?assertMatch(#notify_join{player = Id}, sim_client:head(Key)),
  join_and_start_game(T, SN + 1).

