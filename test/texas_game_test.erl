-module(texas_game_test).
-compile([export_all]).

-include("common.hrl").
-include("schema.hrl").
-include("game.hrl").
-include("protocol.hrl").

-include_lib("eunit/include/eunit.hrl").

start_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
        ?_assert(is_pid(?LOOKUP_GAME(1)))
      ]}}.

list_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
        fun () ->
            ?assert(is_pid(?LOOKUP_GAME(1))),
            ?assert(is_pid(?LOOKUP_GAME(2))),
            timer:sleep(1000),

            [#notify_game{}|[#notify_game{}|[]]] = game:list()
        end
      ]}}.

info_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
        fun () ->
            ?assert(is_pid(?LOOKUP_GAME(1))),
            #notify_game{require = R, seats = C} = game:info(?LOOKUP_GAME(1)),
            ?assertEqual(2, R),
            ?assertEqual(9, C)
        end
      ]}}.

game_query_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
        ?_assert(is_list(protocol:write(#cmd_query_game{})))
      ]}}.

game_info_test_() -> {spawn, {setup, fun setup/0, fun cleanup/1, [
        fun () ->
            ?assert(is_list(protocol:write(#notify_game{
                    game = 1,
                    name = <<"TEXAS_TABLE">>,
                    limit = #limit{min = 10, max = 400, small = 5, big = 10},
                    seats = 9,
                    require = 2,
                    joined = 1}
                )))
        end
      ]}}.

setup() ->
  Conf = #tab_game_config{
    module = game, mods = [{wait_players, []}], limit = no_limit, 
    seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 2},

  schema_test:init(),
  game:start(Conf).

cleanup([]) ->
  ok;
cleanup([H|T]) ->
  exch:stop(H),
  cleanup(T).
