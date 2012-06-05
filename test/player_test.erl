-module(player_test).
-compile([export_all]).

-include("common.hrl").
-include("protocol.hrl").
-include("schema.hrl").

-include_lib("eunit/include/eunit.hrl").

%start_all_test() ->
  %setup(),
  %start("player_1"),
  %?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER(1))),
  %Pdata = pdata(1),
  %?assertEqual(<<"player1">>, Pdata#pdata.nick),
  %?assertEqual(<<"default1">>, Pdata#pdata.photo),
  %[Xref] = mnesia:dirty_read(tab_player, Pdata#pdata.pid),
  %?assertEqual(Pdata#pdata.self, Xref#tab_player.process),
  %?assertEqual(undefined, Xref#tab_player.socket).

start_test() ->
  setup(),
  {ok, _Pid} = player:start("player_1"),
  ?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER(1))),
  PL = plist(1),
  ?assertEqual(<<"player1">>, proplists:get_value(nick, PL)),
  ?assertEqual(<<"default1">>, proplists:get_value(photo, PL)),
  [Xref] = mnesia:dirty_read(tab_player, proplists:get_value(pid, PL)),
  ?assertEqual(Xref#tab_player.process, proplists:get_value(proc, PL)),
  ?assertEqual(undefined, Xref#tab_player.socket).

%auth_test() ->
  %setup(),
  %[R] = mnesia:dirty_index_read(tab_player_info, "player_1", identity),
  %?assertEqual({ok, pass, R}, player:auth("player_1", ?DEF_PWD)),
  %?assertEqual({ok, unauth}, player:auth("player_nil", ?DEF_PWD)),
  %?assertEqual({ok, unauth}, player:auth("player_1", "bad_pwd")),

  %Info = #tab_player_info{identity = "player_1", password = ?DEF_HASH_PWD, disabled = false},
  %?assertEqual({ok, pass, Info}, player:auth(Info, ?DEF_PWD)),
  %?assertEqual({ok, unauth}, player:auth(Info, "bad_pwd")),
  %?assertEqual({ok, player_disable}, player:auth(Info#tab_player_info{disabled = true}, ?DEF_PWD)).

setup() ->
  schema_test:init(),

  Players = [
    #tab_player_info {
      pid = 1,
      identity = "player_1",
      password = ?DEF_HASH_PWD,
      nick = "player1",
      photo = "default1"
    }, #tab_player_info {
      pid = 2,
      identity = "player_2",
      nick = "player2"
    }, #tab_player_info {
      pid = 3,
      identity = "player_3",
      nick = "player3"
    }, #tab_player_info {
      pid = 4,
      identity = "player_4",
      nick = "player4"
    }
  ],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players),
  
  Fun = fun(#tab_player_info{pid = PId}, _Acc) ->
      case ?LOOKUP_PLAYER(PId) of
        Pid when is_pid(Pid) -> 
          ok = gen_server:call(Pid, kill);
        _ -> ok
      end
  end,
  lists:foldl(Fun, nil, Players).

plist(Id) ->
  gen_server:call(?PLAYER(Id), plist).
