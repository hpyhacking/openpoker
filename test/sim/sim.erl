-module(sim).
-compile([export_all]).
-include("genesis.hrl").
-include("genesis_test.hrl").

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

check_notify_leave(_Actor, []) -> ok;
check_notify_leave(Actor, PL = [{Key, _SN}|T]) ->
  {Actor, SN} = proplists:lookup(Actor, PL),
  ?assertMatch(#notify_leave{game = ?GAME, sn = SN}, sim_client:head(Key)),
  check_notify_leave(Actor, T).

check_notify_fold(_SN, []) -> ok;
check_notify_fold(SN, [{Key, _Id}|T]) ->
  ?assertMatch(#notify_fold{game = ?GAME, sn = SN}, sim_client:head(Key)),
  check_notify_fold(SN, T).

check_notify_raise(_Call, _Raise, []) -> ok;
check_notify_raise(Call, Raise, [{Key, _Id}|T]) ->
  ?assertMatch(#notify_raise{call = Call, raise = Raise}, sim_client:head(Key)),
  check_notify_raise(Call, Raise, T).

check_notify_actor(_SN, []) -> ok;
check_notify_actor(SN, [{Key, _Id}|T]) ->
  ?assertMatch(#notify_actor{sn = SN}, sim_client:head(Key)),
  check_notify_actor(SN, T).

check_notify_stage(_GS, []) -> ok;
check_notify_stage(GS, [{Key, _}|T]) ->
  ?assertMatch(#notify_stage{stage = GS}, sim_client:head(Key)),
  check_notify_stage(GS, T).

check_notify_stage_end(_GS, []) -> ok;
check_notify_stage_end(GS, [{Key, _}|T]) ->
  ?assertMatch(#notify_stage_end{stage = GS}, sim_client:head(Key)),
  check_notify_stage_end(GS, T).

check_blind([], _, _, _) -> ok;
check_blind([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 10}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 20}, sim_client:head(Key)),
  ?assertMatch(#notify_stage{stage = ?GS_PREFLOP}, sim_client:head(Key)),
  check_blind(T, B, SB, BB).

check_blind_only_seat([], _, _, _) -> ok;
check_blind_only_seat([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  check_blind_only_seat(T, B, SB, BB).

check_blind_only_raise([], _, _, _) -> ok;
check_blind_only_raise([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 10}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 20}, sim_client:head(Key)),
  check_blind_only_raise(T, B, SB, BB).

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

check_notify_game_end([]) -> ok;
check_notify_game_end([{Key, _}|T]) ->
  ?assertMatch(#notify_game_end{game = ?GAME}, sim_client:head(Key)),
  check_notify_game_end(T).

check_notify_game_cancel([]) -> ok;
check_notify_game_cancel([{Key, _}|T]) ->
  ?assertMatch(#notify_game_cancel{game = ?GAME}, sim_client:head(Key)),
  check_notify_game_cancel(T).

check_notify_win(_Player, _Amt, []) -> ok;
check_notify_win(Player, Amt, [{Key, _Id}|T]) -> 
  ?assertMatch(#notify_win{game = ?GAME, player = Player, amount = Amt}, sim_client:head(Key)),
  check_notify_win(Player, Amt, T).

check_notify_out(_Player, []) -> ok;
check_notify_out(Player, [{Key, _Id}|T]) -> 
  ?assertMatch(#notify_out{game = ?GAME, player = Player}, sim_client:head(Key)),
  check_notify_out(Player, T).

turnover_player_raise({Actor, Players}, {Call, Min, Max}, Raise) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_raise{game = ?GAME, amount = Raise}),
  check_notify_raise(Call, Raise, Players).

turnover_player_fold({Actor, Players}, {Call, Min, Max}) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  sim:check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_fold{game = ?GAME}),
  sim:check_notify_fold(SN, Players).

turnover_player_leave({Actor, Players}, {Call, Min, Max}) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  sim:check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_leave{game = ?GAME}),
  sim:check_notify_leave(Actor, Players),
  sim:check_notify_fold(SN, proplists:delete(Actor, Players)).
