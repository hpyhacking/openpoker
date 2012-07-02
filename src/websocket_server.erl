-module(websocket_server).
-export([start_link/3, start_link/4, stop/1, loop/2]).
-export([generate_websocket_accept/1]).

-include("genesis_common.hrl").

-define(WEBSOCKET_PREFIX,
  "HTTP/1.1 101 Switching Protocols\r\n" ++ 
  "Upgrade: websocket\r\nConnection: Upgrade\r\n").

start_link(Host, Port, Loop) when is_list(Host), is_integer(Port) ->
  start_link(?MODULE, Host, Port, Loop).
  
start_link(Name, Host, Port, Loop) when is_list(Host), is_integer(Port) ->
  Fun = fun (Socket) -> ?MODULE:loop(Socket, Loop) end,
  Options = [{ip, Host}, {loop, Fun}, {port, Port}, {name, Name}],
  socket_server:start(Options).

stop(Name) ->
  socket_server:stop(Name).

%%%
%%% websocket protocol
%%%

handshake(Socket, Fun) ->
  inet:setopts(Socket, [{packet, http}]),
  case gen_tcp:recv(Socket, 0) of
    {ok, {http_request, _Method, Path, _Version}} ->
      {abs_path,PathA} = Path,
      check_header(Socket, PathA, [], Fun);
    {error, {http_error, "\r\n"}} ->
      handshake(Socket, Fun);
    {error, {http_error, "\n"}} ->
      handshake(Socket, Fun)
  end.

verify_handshake(Socket, Path, Headers) ->
  case string:to_lower(proplists:get_value('Upgrade',Headers)) of
    "websocket" ->
      send_handshake(Socket, Path, Headers);
    _Other ->
      error_logger:error_msg("Incorrect WebSocket headers. Closing the connection~n"),
      gen_tcp:close(Socket),
      exit(normal)
  end.

send_handshake(Socket, _Path,Headers) ->
  Key = get_header(Headers, "Sec-Websocket-Key"),
  Protocol = get_header(Headers, "Sec-Websocket-Protocol"),
  %Origin = get_header(Headers, "Sec-Websocket-Origin"),
  %Location = proplists:get_value('Host',Headers),
  Accept = generate_websocket_accept(Key),
  Resp = ?WEBSOCKET_PREFIX ++
  "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n" ++
  "Sec-WebSocket-Protocol: " ++ Protocol ++ "\r\n\r\n" ,
  gen_tcp:send(Socket, Resp).

generate_websocket_accept(Key) ->
  base64:encode_to_string(crypto:sha(
    Key ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")).

check_header(Socket, Path, Headers, Fun) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, http_eoh} ->
      verify_handshake(Socket,Path,Headers),
      %% Set packet back to raw for the rest of the connection
      inet:setopts(Socket, [{packet, raw}, {active, true}]),
      loop(Socket, Fun, ?UNDEF);
    {ok, {http_header, _, Name, _, Value}} ->
      check_header(Socket, Path, [{Name, Value} | Headers], Fun);
    _Other ->
      gen_tcp:close(Socket),
      exit(normal)
  end.

get_header([{Key, Val}|T], FindKey) when is_list(Key) ->
  case string:to_lower(Key) == string:to_lower(FindKey) of
    true -> Val;
    _Other -> get_header(T, FindKey)
  end;

get_header([_H|T], FindKey) ->
  get_header(T, FindKey);

get_header([], _FindKey) ->
  ok.

loop(Socket, Fun, ?UNDEF) ->
  LoopData = Fun(connected, ?UNDEF),
  loop(Socket, Fun, LoopData);
loop(Socket, Fun, LoopData) ->
  NewLoopData = receive
    close ->
      gen_tcp:close(Socket),
      exit(normal);
    {tcp, Socket, Bin} ->
      case decode(Bin) of
        tcp_closed ->
          Fun(disconnected, LoopData),
          gen_tcp:close(Socket),
          exit(normal);
        Data ->
          Fun({recv, Data}, LoopData)
      end;
    {send, Bin} when is_binary(Bin) ->
      gen_tcp:send(Socket, encode(Bin)), 
      LoopData;
    Msg ->
      Fun({msg, Msg}, LoopData)
  end,
  loop(Socket, Fun, NewLoopData).

%%%
%%% private
%%%

loop(Socket, Fun) ->
  handshake(Socket, Fun).

encode(Bin) ->
  Bin1 = base64:encode(Bin),
  coding_data(Bin1, size(Bin1)).

decode(Bin) ->
  case Data = decoding_data(Bin) of
    tcp_closed ->
      tcp_closed;
    _ ->
      base64:decode(Data)
  end.

decoding_data(<<_Fin:1, _Rsv:3, 8:4, _/binary>>) -> tcp_closed;
decoding_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 126:7, _Size:16, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []);
decoding_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, _Size:7, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []).

coding_data(Bin, Size) when Size =< 125 ->
  %%       FIN  RSV  OPCODE  MASK  SIZE    DATA
  <<1:1, 0:3, 1:4,    0:1,  Size:7, Bin/binary>>;
coding_data(Bin, Size) ->
  %%       FIN  RSV  OPCODE  MASK  SIZE            DATA
  <<1:1, 0:3, 1:4,    0:1,  126:7, Size:16, Bin/binary>>.

unmask_data([], _MaskKey, _Index, Result) ->
  lists:reverse(Result);
unmask_data([H|T], MaskKey, Index, Result) ->
  Unmask = H bxor binary:at(MaskKey, Index rem 4),
  unmask_data(T, MaskKey, Index + 1, [Unmask|Result]).
