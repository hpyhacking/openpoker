-module(op_exch).
-behaviour(gen_server).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([start_link/3, stop/1, stop/2, cast/2, call/2]).

-include("openpoker.hrl").

behaviour_info(callbacks) -> [
    {id, 0}, 
    {init, 2}, 
    {stop, 1}, 
    {dispatch, 2},
    {call, 2}
  ].

%%%
%%% client
%%%

start_link(Module, Conf, Mods) ->
  Id = Module:id(),
  Pid = gen_server:start_link({global, {Module, Id}}, op_exch, [Module, Id, Conf, Mods], []),
  start_logger(Pid, Id).

start_logger(R = {ok, Pid}, Id) ->
  LogGames = op_common:get_env(log_games, []),
  case lists:member(Id, LogGames) of
    true ->
      op_exch_event_logger:add_handler(Pid);
    false ->
      ok
  end,
  R.

stop(Pid) when is_pid(Pid) ->
  gen_server:cast(Pid, stop).

stop(Module, Id) when is_number(Id) ->
  gen_server:cast({global, {Module, Id}}, stop).

call(Exch, Event) ->
  gen_server:call(Exch, Event).

cast(Exch, Event) ->
  gen_server:cast(Exch, Event).

%%%
%%% callback
%%%

init([Module, Id, Conf, Mods]) ->
  process_flag(trap_exit, true),
  Ctx = Module:init(Id, Conf),

  Data = #exch{
    id = Id,
    module = Module,
    mods = Mods,
    stack = Mods,
    ctx = Ctx,
    conf = Conf
  },

  case init(?UNDEF, Data) of
    {stop, _, NewData} ->
      {stop, NewData};
    {noreply, NewData} ->
      {ok, NewData}
  end.

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast(Msg, Data = #exch{stack = Stack, ctx = Ctx, state = State}) ->
  {Mod, _} = hd(Stack),
  op_exch_event:cast([{mod, Mod}, {state, State}, {msg, Msg}]),

  %io:format("==========================================~n"),
  %io:format("msg: ~p~n", [Msg]),

  case advance(Mod:State(Msg, Ctx), Msg, Data) of
    R = {noreply, #exch{stack = NewStack, state = NewState}} ->
      {NewMod, _} = hd(NewStack),
      op_exch_event:cast([{next_mod, NewMod}, {next_state, NewState}, {msg, Msg}]),
      R;
    R = {stop, normal, _} ->
      R
  end.

handle_call(Msg, _From, Data = #exch{module = Module, ctx = Context}) ->
  {ok, Result, NewContext} = Module:call(Msg, Context),
  {reply, Result, Data#exch{ctx = NewContext}}.

terminate(normal, #exch{module = Module, ctx = Ctx}) ->
  Module:stop(Ctx);
terminate(_, #exch{module = Module, ctx = Ctx}) ->
  Module:stop(Ctx).

handle_info(Msg, Data) ->
  handle_cast(Msg, Data).

code_change(_OldVsn, Data, _Extra) ->
  {ok, Data}.

%%%
%%% private
%%%

init(Msg, Data = #exch{ stack = [{Mod, Params}|_], ctx = Ctx }) ->
  op_exch_event:init(Mod, Msg),
  advance(Mod:start(Params, Ctx), Msg, Data#exch{ state = ?UNDEF }).

%% continue current state
advance({continue, Ctx}, _Msg, Data = #exch{}) ->
  {noreply, Data#exch{ ctx = Ctx }};

%% next new state
advance({next, State, Ctx}, _Msg, Data) ->
  {noreply, Data#exch{ state = State, ctx = Ctx }};

%% skip mod process, post to Module:dispatch.
advance({skip, Ctx}, Msg, Data = #exch{stack = Stack, module = Module}) ->
  {Mod, _} = hd(Stack),
  op_exch_event:advance([{mod, Mod}, {state, Data#exch.state}, {skip, Msg}]),
  case Mod:dispatch(Msg, Ctx) of
    ok ->
      {noreply, Data#exch{ ctx = Module:dispatch(Msg, Ctx) }};
    skip ->
      {noreply, Data}
  end;

%% 
advance({stop, Ctx}, _Msg, Data = #exch{ stack = [_] }) ->
  {stop, normal, Data#exch{ ctx = Ctx, stack = [] }};

%% stop current mod, init next mod
advance({stop, Ctx}, Msg, Data = #exch{ stack = [_|T] }) ->
  init(Msg, Data#exch{ ctx = Ctx, stack = T });

%% repeat current mod, re init mod
advance({repeat, Ctx}, Msg, Data = #exch{}) ->
  init(Msg, Data#exch{ ctx = Ctx });

%% goto first mod
advance({goto, top, Ctx}, Msg, Data = #exch{mods = Mods}) ->
  init(Msg, Data#exch{ ctx = Ctx, stack = Mods});

%% goto specil mod
advance({goto, Mod, Ctx}, Msg, Data = #exch{stack = Stack}) ->
  init(Msg, Data#exch{ ctx = Ctx, stack = trim_stack(Mod, Stack) }).

trim_stack(_Mod, L = [{_LastMod, _}]) -> L;
trim_stack(Mod, L = [{H, _}|_]) when Mod == H -> L;
trim_stack(Mod, [_|T]) -> trim_stack(Mod, T).

