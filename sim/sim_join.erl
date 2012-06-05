-module(sim_join).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-define(DEF_PLAYER, sim_client:player(jack)).
-define(DEF_PLAYER_ID, (sim_client:player(jack))#tab_player_info.pid).
-define(DEF_PLAYER_IDENTITY, (sim_client:player(jack))#tab_player_info.identity).

-include_lib("eunit/include/eunit.hrl").

info_and_balance_query_test() ->
  run_by_login(fun() ->
        send(#cmd_query_player{}),
        send(#cmd_query_balance{}),
        ?assertMatch(#notify_player{nick = <<"Jack">>, photo = <<"default">>}, head()),
        ?assertMatch(#notify_acount{inplay = 0, balance = 0}, head())
    end).

list_test() ->
  run_by_login(fun() ->
        start_game(), % start two game
        send(#cmd_query_game{}),

        % match two game by head
        ?assertMatch(#notify_game{
          name = <<"TEXAS_TABLE">>, 
          limit = #limit{min = 100, max = 500, small = 5, big = 10}, 
          seats = 9, require = 2, joined = 0
        }, head()),
        ?assertMatch(#notify_game{
          name = <<"TEXAS_TABLE">>, 
          limit = #limit{min = 100, max = 500, small = 5, big = 10}, 
          seats = 9, require = 2, joined = 0
        }, head())
    end).

seat_query_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#cmd_query_seats{game = 1}),
        SeatList = box(),
        ?assertEqual(9, length(SeatList)),
        lists:map(fun(R) ->
              ?assertMatch(#notify_seat{state = ?PS_EMPTY, nick = <<"">>, photo = <<"">>}, R)
          end, SeatList)
    end
  ).

watch_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#cmd_query_seats{game = 1}),
        ?assertMatch(9, length(box())),
        ?assertMatch(#texas{observers = []}, game:ctx(1)),
        send(#cmd_watch{game = 1}),
        ?assertMatch(#notify_game_detail{stage = ?GS_CANCEL, pot = 0, joined = 0, seats = 9}, head()),
        [H|_] = (game:ctx(1))#texas.observers,
        ?assertMatch("jack", element(1, H)),
        ?assert(is_pid(element(2, H)))
    end
  ).

unwatch_test() ->
  run_by_login(fun() ->
        start_game(),
        ?assertMatch(#texas{observers = []}, game:ctx(1)),
        send(#cmd_watch{game = 1}),
        Ctx = game:ctx(1), % watched buy not join
        [H|_] = Ctx#texas.observers,
        ?assertMatch("jack", element(1, H)),
        ?assert(is_pid(element(2, H))),
        send(#cmd_unwatch{game = 1}),
        ?assertMatch(#texas{observers = []}, game:ctx(1))
    end).

join_error_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#cmd_join{game = 1, sn = 1, buyin = 5000}), % join big buyin
        ?assertMatch(#notify_game_detail{}, head()), % watched game
        ?assertMatch(#notify_error{error = ?ERR_JOIN_LESS_BALANCE}, head()), % less balance
        %% check game context 
        Ctx = game:ctx(1), % watched buy not join
        [H|_] = Ctx#texas.observers,
        ?assertMatch("jack", element(1, H)),
        ?assert(is_pid(element(2, H))),
        ?assertMatch(#texas{joined = 0}, Ctx)
    end).
        
join_test() ->
  run_by_login(fun() ->
        start_game(),
        ?assertMatch(#texas{observers = []}, game:ctx(1)),
        send(#cmd_join{game = 1, sn = 1, buyin = 500}),

        ?assertMatch(#notify_game_detail{stage = ?GS_CANCEL, pot = 0, joined = 0, seats = 9}, head()),

        %% check game context 
        Ctx = game:ctx(1),
        Seat = seat:get(1, Ctx#texas.seats),
        Process = sim_client:where_player(?DEF_PLAYER_ID),
        [H|_] = Ctx#texas.observers,
        ?assertMatch("jack", element(1, H)),
        ?assert(is_pid(element(2, H))),
        ?assertMatch(#texas{joined = 1}, Ctx),
        ?assertMatch(#seat{process = Process, identity = "jack", photo = <<"default">>, nick = <<"Jack">>}, Seat),
        ?assertMatch(#notify_join{game = 1, player = 1, buyin = 500, sn = 1, photo = <<"default">>, nick = <<"Jack">>}, head())
    end
  ).

join_leave_acount_test() ->
  run_by_login(fun() ->
        start_game(),
        send(#cmd_join{game = 1, sn = 1, buyin = 500}),
        %% check db
        PId = ?DEF_PLAYER_ID,
        ?assertMatch([#tab_inplay{pid = PId, inplay = 500}], mnesia:dirty_read(tab_inplay, PId)),
        ?assertMatch([#tab_buyin_log{aid = "root", pid = PId, gid = 1, amt = -500, cash = -500, credit = 1000}], mnesia:dirty_index_read(tab_buyin_log, PId, pid)),
        ?assertMatch([#tab_player_info{credit = 1000, cash = -500}], mnesia:dirty_read(tab_player_info, PId)),
        %% check player info
        ?assertMatch(#tab_player_info{credit = 1000, cash = -500}, player:ctx(PId, info)),

        send(#cmd_leave{game = 1}),
        ?assertMatch([], mnesia:dirty_read(tab_inplay, PId)),
        ?assertMatch([#tab_player_info{credit = 1000, cash = 0}], mnesia:dirty_read(tab_player_info, PId))
    end
  ).


run_by_login(Fun) ->
  schema:init(),
  mnesia:dirty_write(?DEF_PLAYER),

  sim_client:kill_games(),
  sim_client:kill_player(?DEF_PLAYER_ID),

  sim_client:start(?MODULE),
  sim_client:send(?MODULE, #cmd_login{identity = list_to_binary(?DEF_PLAYER_IDENTITY), password = <<?DEF_PWD>>}),

  ?assertMatch(#notify_player{}, sim_client:head(?MODULE)),
  ?assertMatch(#notify_acount{}, sim_client:head(?MODULE)),

  Fun().

start_game() ->
  Limit = #limit{min = 100, max = 500, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}], limit = Limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},
  game:start(Conf).
  
where() ->
  sim_client:where(?MODULE).

send(R) ->
  sim_client:send(?MODULE, R).

head() ->
  sim_client:head(?MODULE).

box() ->
  sim_client:box(?MODULE).
