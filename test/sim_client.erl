-module(sim_client).
-export([start/1, start_robot/1, stop/1, send/2, head/1, box/0, box/1, loopdata/2, player/2, loop/2, loop/3, robot_loop/2, robot_loop/3]).
-include("genesis.hrl").

-record(pdata, {
    box = [],
    host = ?UNDEF
  }).

-record(robot_data, { id, game }).

%%%
%%% client
%%%

start(Key) when is_atom(Key) ->
  undefined = whereis(Key),
  PID = spawn(?MODULE, loop, [fun client:loop/2, self()]),
  true = register(Key, PID),
  PID.

start_robot(Key) ->
  undefined = whereis(Key),
  PID = spawn(?MODULE, robot_loop, [fun client:loop/2, Key]),
  true = register(Key, PID),
  PID.

stop(Id) ->
  catch whereis(Id) ! kill,
  ?SLEEP.

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

player(Identity, Players) when is_atom(Identity) ->
  proplists:get_value(Identity, Players).

%%%
%%% callback
%%%

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
