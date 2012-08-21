-module(genesis_game_handler).
-behaviour(webtekcos).

-export([connect/0, connect/1, disconnect/1, handle_message/2, handle_data/2]).

-export([send/1, send/2]).

-include("genesis.hrl").

-record(pdata, { 
    connection_timer = ?UNDEF, 
    player = ?UNDEF,
    player_info = ?UNDEF
  }).

connect() ->
  connect(?CONNECT_TIMEOUT).

connect(ConnectTimeout) ->
  Timer = erlang:start_timer(ConnectTimeout, self(), ?MODULE),
  #pdata{connection_timer = Timer}.

disconnect(#pdata{player_info = Info, player = Player}) when is_pid(Player) ->
  genesis_players_sup:terminate_child(Info#tab_player_info.pid);

disconnect(_) ->
  ok.

handle_message({timeout, _, ?MODULE}, LoopData = #pdata{connection_timer =T}) when T =:= ?UNDEF ->
  LoopData;
  
handle_message({timeout, _, ?MODULE}, _LoopData) ->
  send(#notify_error{error = ?ERR_CONNECTION_TIMEOUT}),
  webtekcos:close().

handle_data(Code, LoopData) when is_list(Code) ->
  Data = base64:decode(list_to_binary(Code)),
  case catch protocol:read(Data) of
    {'EXIT', {_Reason, _Stack}} ->
      send(#notify_error{error = ?ERR_DATA}),
      webtekcos:close();
    R ->
      handle_protocol(R, LoopData)
  end.

%%%%
%%%% handle internal protocol
%%%% 

handle_protocol(R = #cmd_login{}, LoopData = #pdata{connection_timer =T}) when T /= ?UNDEF ->
  catch erlang:cancel_connection_timer(T),
  handle_protocol(R, LoopData#pdata{connection_timer = ?UNDEF});

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
      case genesis_players_sup:start_child(Info) of
        {ok, Player} when is_pid(Player) ->
          player:client(Player),
          player:info(Player),
          player:balance(Player),
          LoopData#pdata{player = Player, player_info = Info}
      end
  end;

handle_protocol(#cmd_logout{}, #pdata{player = Player, player_info = Info}) when is_pid(Player) ->
  genesis_players_sup:terminate_child(Info#tab_player_info.pid),
  webtekcos:close();

handle_protocol(#cmd_query_game{}, LoopData = #pdata{player = Player}) when is_pid(Player) -> 
  lists:map(fun(Info) -> webtekcos:send(Info) end, game:list()), LoopData;

handle_protocol(R, LoopData = #pdata{player = Player}) when is_pid(Player) ->
  player:cast(Player, R), LoopData;

handle_protocol(_R, _LoopData) ->
  send(#notify_error{error = ?ERR_PROTOCOL}),
  webtekcos:close().

send(R) ->
  Data = base64:encode(list_to_binary(protocol:write(R))),
  webtekcos:send_data(Data).

send(PID, R) ->
  Data = base64:encode(list_to_binary(protocol:write(R))),
  webtekcos:send_data(PID, Data).
