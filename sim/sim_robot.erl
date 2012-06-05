-module(sim_robot).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include("game.hrl").

-define(GAME, 1).
-define(GAME_CTX, game:ctx(1)).
-define(JACK, jack).
-define(JACK_ID, 1).
-define(TOMMY, tommy).
-define(TOMMY_ID, 2).
-define(FOO, foo).
-define(FOO_ID, 3).

-include_lib("eunit/include/eunit.hrl").

-define(TWO_PLAYERS, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]).

-define(DELAY, 500).
-define(SLEEP, timer:sleep(?DELAY)).

run() ->
  run_by_login_two_players(fun() ->
        sim_client:send(?JACK, #cmd_join{game = ?GAME, buyin = 1000}),
        sim_client:send(?TOMMY, #cmd_join{game = ?GAME, buyin = 1000})
    end).


%%%
%%% private test until
%%%

run_by_login_two_players(Fun) ->
  run_by_login_players(?TWO_PLAYERS, Fun).

run_by_login_players(Players, Fun) ->
  sim_client:kill_games(),

  lists:map(fun({Key, Id}) ->
        mnesia:dirty_write(sim_client:player(Key)),
        Usr = list_to_binary((sim_client:player(Key))#tab_player_info.identity),
        sim_client:kill_player(Id),
        sim_client:start_robot(Key),
        sim_client:send(Key, #cmd_login{identity = Usr, password = <<?DEF_PWD>>})
    end, Players),
  Limit = #limit{min = 100, max = 1000, small = 1, big = 2},
  Conf = #tab_game_config{module = game, mods = default_mods, limit = Limit, seat_count = 9, start_delay = ?DELAY, required = 2, timeout = 1000, max = 1},
  game:start(Conf),
  Fun().
