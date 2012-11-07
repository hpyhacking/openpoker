-module(exch).
-behaviour(gen_server).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([start_link/3, stop/1, stop/2, cast/2, call/2]).

-include("genesis.hrl").

-record(exch, {
    id,
    module,
    state,
    mods,
    stack,
    ctx,
    conf
  }).

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
  gen_server:start_link({global, {Module, Id}}, exch, [Module, Id, Conf, Mods], []).

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
  advance(Mod:State(Msg, Ctx), Msg, Data).

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
  advance(Mod:start(Params, Ctx), Msg, Data#exch{ state = ?UNDEF }).

advance({continue, Data, Ctx}, _Msg, Data = #exch{}) ->
  {noreply, Data#exch{ ctx = Ctx }};

advance({next, State, Ctx}, _Msg, Data) ->
  {noreply, Data#exch{ state = State, ctx = Ctx }};

advance({skip, Ctx}, Msg, Data = #exch{module = Module}) ->
  {noreply, Data#exch{ ctx = Module:dispatch(Msg, Ctx) }};

advance({stop, Ctx}, _Msg, Data = #exch{ stack = [_] }) ->
  {stop, normal, Data#exch{ ctx = Ctx, stack = [] }};

advance({stop, Ctx}, Msg, Data = #exch{ stack = [_|T] }) ->
  init(Msg, Data#exch{ ctx = Ctx, stack = T });

advance({repeat, Ctx}, Msg, Data = #exch{}) ->
  init(Msg, Data#exch{ ctx = Ctx });

advance({goto, top, Ctx}, Msg, Data = #exch{mods = Mods}) ->
  init(Msg, Data#exch{ ctx = Ctx, stack = Mods});

advance({goto, Mod, Ctx}, Msg, Data = #exch{stack = Stack}) ->
  init(Msg, Data#exch{ ctx = Ctx, stack = trim_stack(Mod, Stack) });

advance(Command, Msg, Data) ->
  ?LOG([{command, Command}, {msg, Msg}, {mod, hd(Data#exch.mods)}, {state, Data#exch.state}, {exch, Data}]),
  {noreply, none}.

trim_stack(_Mod, L = [{_LastMod, _}]) -> L;
trim_stack(Mod, L = [{H, _}|_]) when Mod == H -> L;
trim_stack(Mod, [_|T]) -> trim_stack(Mod, T).
