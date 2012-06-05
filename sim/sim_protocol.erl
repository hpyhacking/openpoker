-module(sim_protocol).
-import(protocol, [write/1, read/1]).
-include("common.hrl").
-include("protocol.hrl").
-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

id_to_game_test() ->
  PID = newp(),
  ?assertEqual(yes, global:register_name({game, 1}, PID)),
  ?assertMatch(#cmd_watch{game = PID}, turn(#cmd_watch{game = 1})).

id_to_player_test() ->
  PID = newp(),
  ?assertEqual(yes, global:register_name({player, 1}, PID)),
  ?assertMatch(#cmd_query_player{player = PID}, turn(#cmd_query_player{player = 1})).

notify_stage_test() ->
  R = turn(#notify_stage{game = 1, stage = 1}),
  ?assertMatch(#notify_stage{game = 1, stage = 1}, R).

notify_win_test() ->
  R = turn(#notify_win{game = 1, player = 1, amount = 100}),
  ?assertMatch(#notify_win{game = 1, player = 1, amount = 100}, R).

%%%
%%% private
%%%

turn(R) ->
  Data = protocol:write(R),
  protocol:read(list_to_binary(Data)).

newp() ->
  spawn(fun loop_fun/0).

loop_fun() ->
  receive _ -> loop_fun() end.
