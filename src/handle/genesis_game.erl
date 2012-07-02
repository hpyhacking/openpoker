-module(genesis_game).
-export([connect/0, connect/1, disconnect/1, handle_message/2, hanlde_data/2]).
-export([send/1, send/2]).

-include("genesis.hrl").

-record(pdata, { 
    timer = ?UNDEF, 
    server = global:whereis_name(server),
    player = ?UNDEF 
  }).

connect() ->
  connect(?CONNECT_TIMEOUT).

connect(ConnectTimeout) ->
  #pdata{timer = erlang:start_timer(ConnectTimeout, self(), ?MODULE)}.

disconnect(_) ->
  ok.

handle_message({timeout, _, ?MODULE}, _LoopData) ->
  send(#notify_error{error = ?ERR_CONNECTION_TIMEOUT}),
  webtekcos:close().

hanlde_data(Data, LoopData) when is_binary(Data) ->
  case catch protocol:read(Data) of
    {'EXIT', {Reason, Stack}} ->
      ?LOG([{handle_data, Data}, {error, {Reason, Stack}}]),
      send(#notify_error{error = ?ERR_DATA}),
      webtekcos:close();
    R ->
      handle_protocol(R, LoopData)
  end.

handle_protocol(R = #cmd_login{}, LoopData = #pdata{timer =T}) when T /= ?UNDEF ->
  catch erlang:cancel_timer(T),
  handle_protocol(R, LoopData#pdata{timer = ?UNDEF});

handle_protocol(#cmd_login{identity = Identity, password = Password}, LoopData) ->
  case player:auth(binary_to_list(Identity), binary_to_list(Password)) of
    {ok, unauth} ->
      send(#notify_error{error = ?ERR_UNAUTH}),
      webtekcos:close();
    {ok, player_disable} ->
      send(#notify_error{error = ?ERR_PLAYER_DISABLE}),
      webtekcos:close();
    {ok, pass, Info} ->
      % create player process by client process, 
      % receive {'EXIT'} when player process error
      case player:start(Info) of
        {ok, Player} when is_pid(Player) ->
          player:client(Player),
          player:info(Player),
          player:balance(Player),
          LoopData#pdata{player = Player}
      end
  end;

handle_protocol(#cmd_logout{}, #pdata{player = Player}) when is_pid(Player) ->
  {ok, logout} = player:logout(Player),
  webtekcos:close();

handle_protocol(#cmd_logout{}, _LoopData) ->
  send(#notify_error{error = ?ERR_PROTOCOL}),
  webtekcos:close();

handle_protocol(#cmd_query_game{}, LoopData = #pdata{player = Player}) when is_pid(Player) -> 
  lists:map(fun(Info) -> webtekcos:send(Info) end, game:list()), LoopData;

handle_protocol(R, LoopData = #pdata{player = Player}) when is_pid(Player) ->
  player:cast(Player, R), LoopData;

handle_protocol(_R, _LoopData) ->
  send(#notify_error{error = ?ERR_PROTOCOL}),
  webtekcos:close().

send(R) ->
  webtekcos:send_data(list_to_binary(protocol:write(R))).

send(PID, R) ->
  webtekcos:send_data(PID, list_to_binary(protocol:write(R))).
