-module(sim_timeout_test).
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

watch_empty_game_test() ->
  run_by_login_two_players(fun() ->
        sim_client:send(?JACK, #cmd_watch{game = ?GAME}),
        ?assertMatch(#notify_game_detail{stage = ?GS_CANCEL, pot = 0, joined = 0, seats = 9}, sim_client:head(?JACK)),

        Ctx = game:ctx(1),
        [H|_] = Ctx#texas.observers,
        ?assertMatch("jack", element(1, H)),
        ?assert(is_pid(element(2, H))),
        timer:sleep(3000),
        ?assertMatch(#notify_game_cancel{game = ?GAME}, sim_client:head(?JACK))
    end).

run_by_login_two_players(Fun) ->
  schema_test:init(),
  mnesia:dirty_write(sim_client:player(?JACK)),
  mnesia:dirty_write(sim_client:player(?TOMMY)),

  sim_client:kill_games(),

  %% login Jack & Tommy
  lists:map(fun({Key, Id}) ->
        Usr = list_to_binary((sim_client:player(Key))#tab_player_info.identity),
        sim_client:kill_player(Id),
        sim_client:start(Key),
        sim_client:send(Key, #cmd_login{identity = Usr, password = <<?DEF_PWD>>}),
        ?assertMatch(#notify_player{}, sim_client:head(Key)),
        ?assertMatch(#notify_acount{}, sim_client:head(Key))
    end, [{?JACK, ?JACK_ID}, {?TOMMY, ?TOMMY_ID}]),

  Limit = #limit{min = 100, max = 400, small = 5, big = 10},
  Conf = #tab_game_config{module = game, mods = [{wait_players, []}, {restart, []}], limit = Limit, seat_count = 9, start_delay = 2000, required = 2, timeout = 1000, max = 1},
    
  game:start(Conf),
  Fun().
