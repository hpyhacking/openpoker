-module(client).
-export([loop/2, send/1, send/2]).

-include("genesis.hrl").

-record(pdata, { 
    timer = ?UNDEF, 
    server = global:whereis_name(server),
    player = ?UNDEF 
  }).

loop(connected, ?UNDEF) ->
  #pdata{timer = erlang:start_timer(?CONNECT_TIMEOUT, self(), ?MODULE)};

loop({connected, _Timeout}, ?UNDEF) ->
  #pdata{timer = erlang:start_timer(?CONNECT_TIMEOUT, self(), ?MODULE)};

loop(disconnected, _Data = #pdata{}) -> ok;

loop({loopdata, player}, Data = #pdata{}) ->
  Data#pdata.player;

loop({recv, Bin}, Data = #pdata{}) when is_binary(Bin) ->
  case catch protocol:read(Bin) of
    {'EXIT', {Reason, Stack}} ->
      ?LOG([{recv, Bin}, {error, {Reason, Stack}}]),
      err(?ERR_DATA);
    R ->
      loop({protocol, R}, Data)
  end;

% cancel connection timer when remote client first send protocol must #login
loop({protocol, R = #cmd_login{}}, Data = #pdata{timer = T}) when T /= ?UNDEF ->
  catch erlang:cancel_timer(T),
  loop({protocol, R}, Data#pdata{timer = ?UNDEF});

loop({protocol, #cmd_login{identity = Identity, password = Password}}, Data) ->
  case player:auth(binary_to_list(Identity), binary_to_list(Password)) of
    {ok, unauth} ->
      err(?ERR_UNAUTH);
    {ok, player_disable} ->
      err(?ERR_PLAYER_DISABLE);
    {ok, pass, Info} ->
      % create player process by client process, 
      % receive {'EXIT'} when player process error
      case player:start(Info) of
        {ok, Player} when is_pid(Player) ->
          player:client(Player),
          player:info(Player),
          player:balance(Player),
          Data#pdata{player = Player}
      end
  end;

loop({protocol, #cmd_logout{}}, #pdata{player = Player}) when is_pid(Player) ->
  {ok, logout} = player:logout(Player),
  close_connection();

loop({protocol, #cmd_logout{}}, _Data) ->
  err(?ERR_PROTOCOL);

loop({protocol, #cmd_query_game{}}, Data = #pdata{player = Player}) when is_pid(Player) ->
  Infos = game:list(),
  lists:map(fun(Info) -> send(Info) end, Infos),
  Data;

loop({protocol, R}, Data = #pdata{player = Player}) when is_pid(Player) ->
  player:cast(Player, R),
  Data;

loop({msg, {timeout, _, ?MODULE}}, _Data) ->
  err(?ERR_CONNECTION_TIMEOUT).

%%%
%%% client
%%%

send(R) ->
  genesis_game_handler:send(R).

send(PID, R) when is_pid(PID), is_tuple(R) ->
  genesis_game_handler:send(PID, R).

%%%
%%% private
%%%

err(Code) when is_integer(Code) ->
  send(#notify_error{error = Code}),
  close_connection().

close_connection() ->
  self() ! close.
