-module(sim_betting).
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
-define(THREE_PLAYERS, ?TWO_PLAYERS ++ [{?FOO, ?FOO_ID}]).

-define(DELAY, 500).
-define(SLEEP, timer:sleep(?DELAY)).

%%%
%%% test case
%%%

normal_betting_test() ->
  run_by_login_players([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}], ?THREE_PLAYERS, fun() ->
        Players = ?THREE_PLAYERS,
        join_and_start_game(Players),

        %% SB 10 BB 20
        B = 1, SB = 2, BB = 3, 
        check_blind(Players, B, SB, BB),

        %% B CALL 20
        check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 80}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(20, 0, Players),

        %% SB CALL 10
        check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 10, min = 20, max = 80}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(10, 0, Players),

        %% BB RAISE 20
        check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 80}, sim_client:head(?FOO)),
        sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 20}),
        check_notify_raise(0, 20, Players),

        %% B CALL 20
        check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(20, 0, Players),

        %% SB CALL 20
        check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 60}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(20, 0, Players),

        %% TURNOVER STAGE
        check_notify_stage_end(?GS_PREFLOP, Players),
        check_notify_stage(?GS_FLOP, Players),

        %% SB CHECK
        check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(0, 0, Players),

        %% BB RAISE 20
        check_notify_actor(BB, Players),
        ?assertMatch(#notify_betting{call = 0, min = 20, max = 60}, sim_client:head(?FOO)),
        sim_client:send(?FOO, #cmd_raise{game = ?GAME, amount = 20}),
        check_notify_raise(0, 20, Players),

        %% B CALL 20
        check_notify_actor(B, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 40}, sim_client:head(?JACK)),
        sim_client:send(?JACK, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(20, 0, Players),

        %% SB CALL
        check_notify_actor(SB, Players),
        ?assertMatch(#notify_betting{call = 20, min = 20, max = 40}, sim_client:head(?TOMMY)),
        sim_client:send(?TOMMY, #cmd_raise{game = ?GAME, amount = 0}),
        check_notify_raise(20, 0, Players),

        %% FLOP OVER
        check_notify_stage_end(?GS_FLOP, Players),
        ?assertMatch(stop, game:state(?GAME))
    end).

normal_betting_and_fold_test() ->
  run_by_login_players([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}], ?THREE_PLAYERS, fun() ->
        B = 1, SB = 2, BB = 3,
        Players = set_sn([{?JACK, B}, {?TOMMY, SB}, {?FOO, BB}], ?THREE_PLAYERS),

        join_and_start_game(Players),
        check_blind(Players, B, SB, BB),

        turnover_player_raise({?JACK, Players},  {20, 20, 80}, 0),
        turnover_player_raise({?TOMMY, Players}, {10, 20, 80}, 0),
        turnover_player_raise({?FOO, Players},   { 0, 20, 80}, 0),

        check_notify_stage_end(?GS_PREFLOP, Players),
        check_notify_stage(?GS_FLOP, Players),

        turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 20),
        turnover_player_fold ({?FOO, Players},   {20, 20, 60}),
        turnover_player_raise({?JACK, Players},  {20, 20, 60}, 0),

        check_notify_stage_end(?GS_FLOP, Players),
        ?assertMatch(#texas{joined = 3}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end).

normal_betting_and_leave_test() ->
  run_by_login_players([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}], ?THREE_PLAYERS, fun() ->
        B = 1, SB = 2, BB = 3,
        Players = set_sn([{?JACK, B}, {?TOMMY, SB}, {?FOO, BB}], ?THREE_PLAYERS),

        join_and_start_game(Players),
        check_blind(Players, B, SB, BB),

        turnover_player_raise({?JACK, Players},  {20, 20, 80}, 0),
        turnover_player_raise({?TOMMY, Players}, {10, 20, 80}, 0),
        turnover_player_raise({?FOO, Players},   { 0, 20, 80}, 0),

        check_notify_stage_end(?GS_PREFLOP, Players),
        check_notify_stage(?GS_FLOP, Players),

        turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 20),
        turnover_player_leave({?FOO, Players},   {20, 20, 60}),
        turnover_player_raise({?JACK, proplists:delete(?FOO, Players)},  {20, 20, 60}, 0),

        check_notify_stage_end(?GS_FLOP, proplists:delete(?FOO, Players)),
        ?assertMatch(#texas{joined = 2}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end).

headsup_betting_test() ->
  run_by_login_two_players([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}], fun() ->
        SB = 1, BB = 2,
        Players = set_sn([{?JACK, SB}, {?TOMMY, BB}], ?TWO_PLAYERS),

        join_and_start_game(Players),
        check_blind(Players, SB, SB, BB),

        turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        turnover_player_raise({?TOMMY, Players}, { 0, 20, 80}, 0),

        check_notify_stage_end(?GS_PREFLOP, Players),
        check_notify_stage(?GS_FLOP, Players),

        turnover_player_raise({?TOMMY, Players}, {0, 20, 80}, 0),
        turnover_player_raise({?JACK, Players},  {0, 20, 80}, 20),
        turnover_player_raise({?TOMMY, Players}, {20, 20, 60}, 0),

        check_notify_stage_end(?GS_FLOP, Players),
        ?assertMatch(stop, game:state(?GAME))
    end).

headsup_betting_and_fold_test() ->
  run_by_login_two_players([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}], fun() ->
        SB = 1, BB = 2,
        Players = set_sn([{?JACK, SB}, {?TOMMY, BB}], ?TWO_PLAYERS),

        join_and_start_game(Players),
        check_blind(Players, SB, SB, BB),

        turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        turnover_player_fold({?TOMMY, Players},  { 0, 20, 80}),

        ?assertMatch(#texas{joined = 2}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end).

headsup_betting_and_leave_test() ->
  run_by_login_two_players([{blinds, []}, {betting, [?GS_PREFLOP]}, {betting, [?GS_FLOP]}], fun() ->
        SB = 1, BB = 2,
        Players = set_sn([{?JACK, SB}, {?TOMMY, BB}], ?TWO_PLAYERS),

        join_and_start_game(Players),
        check_blind(Players, SB, SB, BB),

        turnover_player_raise({?JACK, Players},  {10, 20, 80}, 0),
        turnover_player_leave({?TOMMY, Players}, { 0, 20, 80}),


        ?assertMatch(#texas{joined = 1}, game:ctx(?GAME)),
        ?assertMatch(stop, game:state(?GAME))
    end).

%%%
%%% private test until
%%%

turnover_player_fold({Actor, Players}, {Call, Min, Max}) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_fold{game = ?GAME}),
  check_notify_fold(SN, Players).

turnover_player_leave({Actor, Players}, {Call, Min, Max}) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_leave{game = ?GAME}),
  check_notify_leave(Actor, Players),
  check_notify_fold(SN, proplists:delete(Actor, Players)).

turnover_player_raise({Actor, Players}, {Call, Min, Max}, Raise) ->
  {Actor, SN} = proplists:lookup(Actor, Players),
  check_notify_actor(SN, Players),
  ?assertMatch(#notify_betting{call = Call, min = Min, max = Max}, sim_client:head(Actor)),
  sim_client:send(Actor, #cmd_raise{game = ?GAME, amount = Raise}),
  check_notify_raise(Call, Raise, Players).

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

check_blind([], _, _, _) -> ok;
check_blind([{Key, _Id}|T], B, SB, BB) ->
  ?assertMatch(#notify_button{b = B}, sim_client:head(Key)),
  ?assertMatch(#notify_sb{sb = SB}, sim_client:head(Key)),
  ?assertMatch(#notify_bb{bb = BB}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 10}, sim_client:head(Key)),
  ?assertMatch(#notify_raise{call = 20}, sim_client:head(Key)),
  ?assertMatch(#notify_stage{stage = ?GS_PREFLOP}, sim_client:head(Key)),
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

set_sn([], Players) -> Players;
set_sn([{Key, SN}|T], Players) ->
  lists:keyreplace(Key, 1, Players, {Key, SN}),
  set_sn(T, Players).

