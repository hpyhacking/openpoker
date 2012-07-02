-module(sim_client).
-export([start/1, start_robot/1, stop/1, send/2, head/1, box/0, box/1, loopdata/2, player/2, loop/2, loop/3, robot_loop/2, robot_loop/3, setup_players/1]).

%% 模拟网络链接到服务器
%% 通过启动一个进程，模拟对网络链接的双向请求，
%% 提供单元测试一种模拟客户端的机制。

%% 由于使用webtekcos提供的wesocket链接，通过genesis_game模块
%% 处理客户端与服务器间通信的各种消息。sim_client启动后同样模拟
%% webtekcos的消息层，将各种消息同样发到genesis_game模块进行处理
%% 以达到Mock的效果。同时sim_client为测试程序提供了一些格外的接口
%% 以检查通信进程内部进程数据的正确性。

%% sim_client采用erlang最基本的消息元语进行编写。

-include("genesis.hrl").
-include("genesis_test.hrl").

-record(pdata, { box = [] }).
-record(robot_data, { id, game }).

%%%
%%% client
%%%

start(Key) when is_atom(Key) ->
  undefined = whereis(Key),
  PID = spawn(?MODULE, loop, [genesis_game]),
  true = register(Key, PID),
  PID.

stop(Id) when is_pid(Id) ->
  catch Id ! kill,
  ?SLEEP;

stop(Id) when is_atom(Id) ->
  stop(whereis(Id)),
  case player(Id, ?PLAYERS) of
    undefined ->
      ok;
    Player ->
      player:stop(Player#tab_player_info.pid)
  end.

send(Id, R) ->
  Id ! {sim, send, R},
  ?SLEEP.

head(Id) ->
  Id ! {sim, head, self()},
  receive 
    R when is_tuple(R) -> R
  after
    500 -> exit(request_timeout)
  end.

box(Id) ->
  Id ! {sim, box, self()},
  receive 
    Box when is_list(Box) -> Box
  after
    500 -> exit(request_timeout)
  end.

%% tools function

player(Identity, Players) when is_atom(Identity) ->
  proplists:get_value(Identity, Players).

setup_players(L) when is_list(L) ->
  lists:map(fun ({Key, R}) ->
        ?assertNot(is_pid(whereis(Key))),
        Identity = list_to_binary(R#tab_player_info.identity),
        mnesia:dirty_write(R),
        sim_client:start(Key),
        sim_client:send(Key, #cmd_login{identity = Identity, password = <<?DEF_PWD>>}),
        ?assertMatch(#notify_player{}, sim_client:head(Key)),
        ?assertMatch(#notify_acount{}, sim_client:head(Key)),
        {Key, R}
    end, L).

%%%
%%% callback
%%%

loop(Mod, ?UNDEF, Data = #pdata{}) ->
  LoopData = Mod:connect(60 * 1000),
  loop(Mod, LoopData, Data);

loop(Mod, LoopData, Data = #pdata{box = Box}) ->
  receive
    %% sim send protocol from client to server
    {sim, send, R} when is_tuple(R) ->
      NewLoopData = Mod:handle_data(list_to_binary(protocol:write(R)), LoopData),
      loop(Mod, NewLoopData, Data);
    %% sim get client side header message
    {sim, head, From} when is_pid(From) ->
      case Box of
        [H|T] ->
          From ! H,
          loop(Mod, LoopData, Data#pdata{box = T});
        [] ->
          loop(Mod, LoopData, Data#pdata{box = []})
      end;
    {sim, box, From} when is_pid(From) ->
      From ! Box,
      loop(Mod, LoopData, Data#pdata{box = []});
    {sim, kill} ->
      exit(kill);

    close ->
      Data#pdata.host ! Box,
      exit(normal);
    {send, Bin} when is_binary(Bin) ->
      R = protocol:read(Bin),
      loop(Mod, LoopData, Data#pdata{box = Box ++ [R]});
    Message ->
      NewLoopData = Mod:handle_message(Message, LoopData),
      loop(Mod, NewLoopData, Data)
  end.









%start_robot(Key) ->
  %undefined = whereis(Key),
  %PID = spawn(?MODULE, robot_loop, [fun client:loop/2, Key]),
  %true = register(Key, PID),
  %PID.

%robot_loop(Fun, Id) ->
  %LoopData = Fun({connected, 0}, ?UNDEF),
  %robot_loop(Fun, LoopData, #robot_data{id = Id}).

%robot_loop(Fun, LoopData, Data = #robot_data{id = Id}) ->
  %receive
    %{send, Bin} when is_binary(Bin) ->
      %case protocol:read(Bin) of
        %#notify_game_start{game = Game} ->
          %robot_loop(Fun, LoopData, Data#robot_data{game = Game});
        %R = #notify_betting{call = Call, min = Min} ->
          %io:format("BETTING ~p:~p ~p~n", [Id, element(1,R), R]),
          %timer:sleep(100),
          %case Call of
            %0 ->
              %send(Id, #cmd_raise{game = Data#robot_data.game, amount = Min});
            %Call ->
              %send(Id, #cmd_raise{game = Data#robot_data.game, amount = 0})
          %end,
          %robot_loop(Fun, LoopData, Data);
        %R ->
          %io:format("PROTOCOL ~p:~p ~p~n", [Id, element(1,R), R]),
          %robot_loop(Fun, LoopData, Data)
      %end;
    %{send, R} when is_tuple(R) ->
      %NewLoopData = Fun({recv, list_to_binary(protocol:write(R))}, LoopData),
      %robot_loop(Fun, NewLoopData, Data);
    %_ ->
      %ok
  %end.

