-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, start/2, stop/0]).

-include("genesis.hrl").

-record(pdata, { port, host }).

%%%
%%% callback
%%%

init([Host, Port]) ->
  process_flag(trap_exit, true), 
  {ok, _} = websocket_server:start(Host, Port, fun client:loop/2),
  {ok, #pdata{ host = Host, port = Port }}.

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast(Msg, Data) ->
  ?LOG([{cast, Msg}]),
  {noreply, Data}.

handle_call({info, server}, _From, Data = #pdata{host = Host, port = Port}) ->
  {reply, {Host, Port}, Data};

handle_call({info, users}, _From, Data) ->
  {reply, 0, Data};

handle_call(Msg, _From, Data) ->
  ?LOG([{call, Msg}]),
  {noreply, Data}.

handle_info({'EXIT', Pid, Reason}, Data) ->
  ?LOG([{exit, Pid}, {reason, Reason}]),
  {noreply, Data};

handle_info(Msg, Data) ->
  ?LOG([{info, Msg}]),
  {noreply, Data}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(_, #pdata{port = Port}) ->
  websocket_server:stop(Port).

%%%
%%% client
%%%

start() ->
  start("127.0.0.1", 8002).

start(Host, Port) ->
  {ok, _Pid} = gen_server:start({global, server}, server, [Host, Port], []).

stop() ->
  gen_server:cast({global, server}, stop).
