-module(protocol_test).
-import(protocol, [write/1, read/1]).
-include("genesis.hrl").
-include("genesis_test.hrl").

-define(REG_GAME, {game, 1}).
-define(REG_PLAYER, {player, 1}).

id_to_game_test_() -> {setup, fun setup_game/0, fun cleanup/1, fun () ->
        ?assert(is_pid((turn(#cmd_watch{game = 1}))#cmd_watch.game))
    end}.

id_to_player_test_() -> {setup, fun setup_player/0, fun cleanup/1, fun () ->
        ?assert(is_pid((turn(#cmd_query_player{player = 1}))#cmd_query_player.player))
    end}.

notify_stage_test() ->
  R = turn(#notify_stage{game = 1, stage = 1}),
  ?assertMatch(#notify_stage{game = 1, stage = 1}, R).

notify_win_test() ->
  R = turn(#notify_win{game = 1, player = 1, amount = 100}),
  ?assertMatch(#notify_win{game = 1, player = 1, amount = 100}, R).

setup_game() ->
  Pid = spawn(fun loop_fun/0),
  ?assertEqual(yes, global:register_name(?REG_GAME, Pid)),
  Pid.

setup_player() ->
  Pid = spawn(fun loop_fun/0),
  ?assertEqual(yes, global:register_name(?REG_PLAYER, Pid)),
  Pid.

cleanup(Pid) ->
  exit(Pid,exit).

turn(R) ->
  Data = protocol:write(R),
  protocol:read(list_to_binary(Data)).

loop_fun() ->
  receive _ -> loop_fun() end.
