-module(genesis_webtekcos_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

add_handler() ->
  webtekcos_event:add_handler(?MODULE, []).

delete_handler() ->
  webtekcos_event:delete_handler(?MODULE, []).

init([]) ->
  {ok, []}.

handle_event({rcv_data, Code}, State) ->
  Data = base64:decode(list_to_binary(Code)),
  case catch protocol:read(Data) of
    {'EXIT', {_Reason, _Stack}} ->
      io:format("GENESIS RECV ERROR_DATA [~p] ~n", [Data]);
    R ->
      io:format("GENESIS RECV ~p~n", [R])
  end,
  {ok, State};

handle_event({send_data, Code}, State) ->
  Data = base64:decode(Code),
  case catch protocol:read(Data) of
    {'EXIT', {_Reason, _Stack}} ->
      io:format("GENESIS SEND ERROR_DATA [~p] ~n", [Data]);
    R ->
      io:format("GENESIS SEND ~p~n", [R])
  end,
  {ok, State};

handle_event({start, _Args}, State) ->
  {ok, State};

handle_event(connect, State) ->
  io:format("GENESIS RECV CONNECTION~n"),
  {ok, State};

handle_event(disconnect, State) ->
  io:format("GENESIS DISCONNECTION~n"),
  {ok, State}.

handle_call(_Request, State) -> 
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.
