-module(sim_client).
-compile([export_all]).

-include("common.hrl").
-include("game.hrl").
-include("schema.hrl").
-include("protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLEEP, timer:sleep(100)).


-record(pdata, {
    box = [],
    host = ?UNDEF
  }).

-record(robot_data, { id, game }).

%%%
%%% client
%%%

start() ->
  start(sim_client).

start(Id) when is_atom(Id) ->
  kill(Id),
  PID = spawn(?MODULE, loop, [fun client:loop/2, self()]),
  true = register(Id, PID),
  PID.

start_robot(Id) ->
  kill(Id),
  PID = spawn(?MODULE, robot_loop, [fun client:loop/2, Id]),
  true = register(Id, PID),
  PID.

kill(Id) ->
  catch where(Id) ! kill,
  ?SLEEP.

kill_games() ->
  kill_games(1),
  ?SLEEP.

kill_games(N) ->
  case kill_game(N) of
    ok -> kill_games(N+1);
    undefined -> ok
  end.

kill_game(Id) ->
  case where_game(Id) of
    Game when is_pid(Game) ->
      gen_server:call(Game, kill);
    undefined ->
      undefined
  end.

kill_player(PId) ->
  case where_player(PId) of
    Player when is_pid(Player) ->
      gen_server:call(Player, kill);
    undefined ->
      undefined
  end.
  
where(Id) ->
  whereis(Id).

where_game(Id) ->
  ?LOOKUP_GAME(Id).

where_player(PId) ->
  ?LOOKUP_PLAYER(PId).
  
send(Id, R) ->
  Id ! {send, R},
  ?SLEEP.

head(Id) ->
  Id ! {head, self()},
  receive 
    R when is_tuple(R) -> R
  after
    500 -> exit(request_timeout)
  end.

box() ->
  receive
    Box when is_list(Box) -> Box
  after
    500 -> exit(request_timeout)
  end.

box(Id) ->
  Id ! {box, self()},
  receive 
    Box when is_list(Box) -> Box
  after
    500 -> exit(request_timeout)
  end.

loopdata(Id, Key) ->
  Id ! {loopdata, Key, self()},
  receive 
    LoopDataVal -> LoopDataVal
  after
    500 -> exit(request_timeout)
  end.

players() ->
  [
    {jack, #tab_player_info{
        pid = 1, 
        identity = "jack", 
        nick = "Jack",
        photo = "default",
        password = ?DEF_HASH_PWD,
        cash = 0,
        credit = 1000,
        disabled = false }},
    {tommy, #tab_player_info{
        pid = 2, 
        identity = "tommy", 
        nick = "Tommy",
        photo = "default",
        password = ?DEF_HASH_PWD,
        cash = 0,
        credit = 1000,
        disabled = false }},
    {foo, #tab_player_info{
        pid = 3, 
        identity = "foo", 
        nick = "Foo",
        photo = "default",
        password = ?DEF_HASH_PWD,
        cash = 0,
        credit = 1000,
        disabled = false }}
  ].

player(Identity) when is_atom(Identity) ->
  {Identity, Data} = proplists:lookup(Identity, players()),
  Data.

%%%
%%% callback
%%%

-define(PUTS(L), error_logger:info_report(L)).

robot_loop(Fun, Id) ->
  LoopData = Fun({connected, 0}, ?UNDEF),
  robot_loop(Fun, LoopData, #robot_data{id = Id}).

robot_loop(Fun, LoopData, Data = #robot_data{id = Id}) ->
  receive
    {send, Bin} when is_binary(Bin) ->
      case protocol:read(Bin) of
        #notify_game_start{game = Game} ->
          robot_loop(Fun, LoopData, Data#robot_data{game = Game});
        R = #notify_betting{call = Call, min = Min} ->
          io:format("BETTING ~p:~p ~p~n", [Id, element(1,R), R]),
          timer:sleep(100),
          case Call of
            0 ->
              send(Id, #cmd_raise{game = Data#robot_data.game, amount = Min});
            Call ->
              send(Id, #cmd_raise{game = Data#robot_data.game, amount = 0})
          end,
          robot_loop(Fun, LoopData, Data);
        R ->
          io:format("PROTOCOL ~p:~p ~p~n", [Id, element(1,R), R]),
          robot_loop(Fun, LoopData, Data)
      end;
    {send, R} when is_tuple(R) ->
      NewLoopData = Fun({recv, list_to_binary(protocol:write(R))}, LoopData),
      robot_loop(Fun, NewLoopData, Data);
    _ ->
      ok
  end.

loop(Fun, Host) ->
  loop(Fun, ?UNDEF, #pdata{host = Host}).

loop(Fun, ?UNDEF, Data = #pdata{}) ->
  LoopData = Fun({connected, 60 * 1000}, ?UNDEF),
  loop(Fun, LoopData, Data);

loop(Fun, LoopData, Data = #pdata{box = Box}) ->
  receive
    kill ->
      exit(kill);
    %% clien module callback close connection.
    close ->
      Data#pdata.host ! Box,
      exit(normal);
    %% clien module callback send bianry to remote client.
    {send, Bin} when is_binary(Bin) ->
      R = protocol:read(Bin),
      NB = Box ++ [R], %% insert new message to box
      loop(Fun, LoopData, Data#pdata{box = NB});
    %% send protocol record to clinet module.
    {send, R} when is_tuple(R) ->
      ND = Fun({recv, list_to_binary(protocol:write(R))}, LoopData),
      loop(Fun, ND, Data); %% sim socket binary data
    %% host process get message box head one.
    {head, From} when is_pid(From) ->
      case Box of
        [H|T] ->
          From ! H,
          loop(Fun, LoopData, Data#pdata{box = T});
        [] ->
          loop(Fun, LoopData, Data#pdata{box = []})
      end;
    {box, From} when is_pid(From) ->
      From ! Box,
      loop(Fun, LoopData, Data#pdata{box = []});
    {loopdata, Key, From} when is_pid(From) ->
      Result = Fun({loopdata, Key}, LoopData),
      From ! Result,
      loop(Fun, LoopData, Data);
    Msg ->
      ND = Fun({msg, Msg}, LoopData),
      loop(Fun, ND, Data)
  end.

