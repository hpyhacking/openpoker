-module(sim_deal).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(GAME, 1).
-define(GAME_CTX, game:ctx(1)).
-define(JACK, jack).
-define(JACK_ID, 1).
-define(TOMMY, tommy).
-define(TOMMY_ID, 2).
-define(FOO, foo).
-define(FOO_ID, 3).

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).

-define(DELAY, 500).
-define(SLEEP, timer:sleep(?DELAY)).

%%%
%%% test case
%%%

private_rank_test() ->
  RigCards = hand:make_cards("3H 4H 3D 4D"),
  run_by_login_two_players([
      {blinds, []}, {rig, [RigCards]}, {deal_cards, [2, private]}, {ranking, []}], fun() ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),
        check_deal(),

        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_THREE}, sim_client:head(?TOMMY))
    end).

share_rank_test() ->
  run_by_login_two_players([
      {blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C")]}, {deal_cards, [2, private]}, {deal_cards, [1, shared]}, {ranking, []}], fun() ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),
        check_deal(),
        check_shared(1, Players),
        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_THREE_KIND, high1 = ?CF_THREE}, sim_client:head(?TOMMY))
    end),
  run_by_login_two_players([
      {blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C 4C 4S ")]}, 
      {deal_cards, [2, private]}, {deal_cards, [3, shared]}, {ranking, []}], fun() ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),
        check_deal(),
        check_shared(3, Players),
        ?assertMatch(#notify_hand{rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?TOMMY))
    end).

deal_test() ->
  run_by_login_two_players([
      {blinds, []}, {deal_cards, [2, private]}, 
      {deal_cards, [1, shared]}, {deal_cards, [1, shared]}, {deal_cards, [1, shared]}], fun() ->
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
    end).

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


%%%
%%% private test until
%%%

check_shared(N, Players) ->
  check_shared(N, N, Players).

check_shared(_, _, []) -> ok;
check_shared(N, 0, [_|T]) -> 
  check_shared(N, N, T);
check_shared(N, S, L = [{Key, _Id}|_T]) ->
  ?assertMatch(#notify_shared{game = ?GAME}, sim_client:head(Key)),
  check_shared(N, S - 1, L).

run_by_login_two_players(Fun) ->
  run_by_login_players([], ?TWO_PLAYERS, Fun).

run_by_login_two_players(Mods, Fun) ->
  run_by_login_players(Mods, ?TWO_PLAYERS, Fun).

run_by_login_players(MixinMods, Players, Fun) ->
  schema:init(),
  mnesia:dirty_write(sim_client:player(?TOMMY)),

  sim_client:kill_games(),

  %% login Jack & Tommy
  lists:map(fun({Key, Id}) ->
        mnesia:dirty_write(sim_client:player(Key)),
        Usr = list_to_binary((sim_client:player(Key))#tab_player_info.identity),
        sim_client:kill_player(Id),
        sim_client:start(Key),
        sim_client:send(Key, #cmd_login{identity = Usr, password = <<?DEF_PWD>>}),
        ?assertMatch(#notify_player{}, sim_client:head(Key)),
        ?assertMatch(#notify_acount{}, sim_client:head(Key))
    end, Players),
  Mods = [{wait_players, []}] ++ MixinMods ++ [{stop, []}],
  Limit = #limit{min = 100, max = 400, small = 10, big = 20},
  Conf = #tab_game_config{module = game, mods = Mods, limit = Limit, seat_count = 9, start_delay = ?DELAY, required = 2, timeout = 1000, max = 1},
    
  game:start(Conf),
  Fun().

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

