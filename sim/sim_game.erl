-module(sim_game).
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

rank_test() ->
  run_by_login_two_players([
      {blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C")]}, {deal_cards, [2, private]}, {deal_cards, [1, shared]}, {ranking, []}], fun() ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),

        check_deal(),
        check_shared(1, Players),
        ?assertMatch(#notify_hand{rank = ?HC_PAIR, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_THREE_KIND, high1 = ?CF_THREE}, sim_client:head(?TOMMY))
    end).

shutdown_test() ->
  Mods = [{blinds, []}, {rig, [hand:make_cards("3H 4H 3D 4D 3C 4C 4S ")]}, 
    {deal_cards, [2, private]}, {deal_cards, [3, shared]}, {ranking, []},
    {betting, [?GS_PREFLOP]}, {showdown, []}, {wait_players, []}],
  run_by_login_two_players(Mods, fun() ->
        Players = ?TWO_PLAYERS,
        join_and_start_game(Players),
        check_blind(Players, 1, 1, 2),
        check_deal(),
        check_shared(3, Players),
        ?assertMatch(#notify_hand{rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?TOMMY)),

        %% betting
        check_notify_stage(?GS_PREFLOP, Players),
        turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 70),
        turnover_player_raise({?JACK, Players},  {70, 70, 10}, 0),
        check_notify_stage_end(?GS_PREFLOP, Players),

        ?assertMatch(#notify_cards{player = ?TOMMY_ID }, sim_client:head(?JACK)),
        ?assertMatch(#notify_cards{player = ?JACK_ID }, sim_client:head(?TOMMY)),

        ?assertMatch(#notify_hand{player = ?TOMMY_ID, rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_hand{player = ?JACK_ID, rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?TOMMY)),
        ?assertMatch(#notify_hand{player = ?TOMMY_ID, rank = ?HC_FULL_HOUSE, high1 = ?CF_THREE, high2 = ?CF_FOUR}, sim_client:head(?JACK)),
        ?assertMatch(#notify_hand{player = ?JACK_ID, rank = ?HC_FOUR_KIND, high1 = ?CF_FOUR}, sim_client:head(?JACK)),

        check_notify_win(?JACK_ID, 40 + 140, Players),
        check_notify_out(?TOMMY_ID, Players),

        ?assertMatch([#tab_inplay{inplay = 190}], mnesia:dirty_read(tab_inplay, ?JACK_ID)),
        ?assertMatch([#tab_inplay{inplay = 10}], mnesia:dirty_read(tab_inplay, ?TOMMY_ID)),
        ?assertMatch([#tab_player_info{cash = -100}], mnesia:dirty_read(tab_player_info, ?JACK_ID)),
        ?assertMatch([#tab_player_info{cash = -100}], mnesia:dirty_read(tab_player_info, ?TOMMY_ID)),

        check_notify_game_end(Players),

        ?SLEEP,

        check_notify_game_cancel(Players),
        check_notify_leave(?TOMMY, Players)
    end).

%%%
%%% private test until
%%%
check_notify_game_end([]) -> ok;
check_notify_game_end([{Key, _}|T]) ->
  ?assertMatch(#notify_game_end{game = ?GAME}, sim_client:head(Key)),
  check_notify_game_end(T).

check_notify_game_cancel([]) -> ok;
check_notify_game_cancel([{Key, _}|T]) ->
  ?assertMatch(#notify_game_cancel{game = ?GAME}, sim_client:head(Key)),
  check_notify_game_cancel(T).

check_notify_leave(_Actor, []) -> ok;
check_notify_leave(Actor, PL = [{Key, _SN}|T]) ->
  {Actor, SN} = proplists:lookup(Actor, PL),
  ?assertMatch(#notify_leave{game = ?GAME, sn = SN}, sim_client:head(Key)),
  check_notify_leave(Actor, T).

check_notify_win(_Player, _Amt, []) -> ok;
check_notify_win(Player, Amt, [{Key, _Id}|T]) -> 
  ?assertMatch(#notify_win{game = ?GAME, player = Player, amount = Amt}, sim_client:head(Key)),
  check_notify_win(Player, Amt, T).

check_notify_out(_Player, []) -> ok;
check_notify_out(Player, [{Key, _Id}|T]) -> 
  ?assertMatch(#notify_out{game = ?GAME, player = Player}, sim_client:head(Key)),
  check_notify_out(Player, T).

check_notify_stage(_GS, []) -> ok;
check_notify_stage(GS, [{Key, _}|T]) ->
  ?assertMatch(#notify_stage{stage = GS}, sim_client:head(Key)),
  check_notify_stage(GS, T).

turnover_player_raise({Actor, Players}, {Call, Min, Max}, Raise) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_raise{game = ?GAME, amount = Raise}),
  check_notify_raise(Call, Raise, Players).

check_notify_raise(_Call, _Raise, []) -> ok;
check_notify_raise(Call, Raise, [{Key, _Id}|T]) ->
  ?assertMatch(#notify_raise{call = Call, raise = Raise}, sim_client:head(Key)),
  check_notify_raise(Call, Raise, T).

check_notify_actor(_SN, []) -> ok;
check_notify_actor(SN, [{Key, _Id}|T]) ->
  ?assertMatch(#notify_actor{sn = SN}, sim_client:head(Key)),
  check_notify_actor(SN, T).

check_notify_stage_end(_GS, []) -> ok;
check_notify_stage_end(GS, [{Key, _}|T]) ->
  ?assertMatch(#notify_stage_end{stage = GS}, sim_client:head(Key)),
  check_notify_stage_end(GS, T).

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
