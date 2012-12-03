-module(op_exch_event_logger).
-behaviour(gen_event).
-export([add_handler/1, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

-include("openpoker.hrl").

add_handler(GID) when is_integer(GID) ->
  op_exch_event:add_handler(?MODULE, [?LOOKUP_GAME(GID)]);

add_handler(GamePID) when is_pid(GamePID) ->
  op_exch_event:add_handler(?MODULE, [GamePID]).

delete_handler() ->
  op_exch_event:delete_handler(?MODULE, []).

init([GamePID]) ->
  {ok, [GamePID]}.

handle_event({log, Msg}, State) ->
  error_logger:info_report([Msg]),
  {ok, State};

handle_event({GamePID, Msg}, State = [GamePID]) ->
  handle_event({log, Msg}, State);

handle_event({_, _Msg}, _State) ->
  remove_handler.
  
handle_call(_Request, State) -> 
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.
