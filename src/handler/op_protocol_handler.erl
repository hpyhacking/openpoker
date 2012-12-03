-module(op_protocol_handler).
-export([connect/0, disconnect/1, handle_message/2, handle_data/2]).
-export([loop/0]).

-include("openpoker.hrl").

connect() -> 
  console(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"),
  put(convert_id_to_process, true).

disconnect(_) ->
  console("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<").

handle_message(Msg, _LoopData) -> console([op_protocol_msg, Msg]).

handle_data(Data, _LoopData) when is_list(Data) ->
  Bin = base64:decode(list_to_binary(Data)),
  console([handle_data, Bin]),
  Result = protocol:read(Bin),
  console([handle_protocol, Result]),

  case Result of
    {'EXIT', {Reason, Stack}} ->
      ?LOG([{handle_data_error, {Reason, Stack}}]),
      send(#notify_error{error = ?ERR_DATA}),
      webtekcos:close();
    R ->
      send(R)
  end.

console(R) ->
  io:format("===> ~p~n", [R]).

send(R) ->
  D = protocol:write(R),
  Bin = list_to_binary(D),
  Encode = base64:encode(Bin),
  webtekcos:send_data(Encode).

loop() ->
  receive 
    _ -> ok
  end.
