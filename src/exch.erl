-module(exch).
-behaviour(gen_server).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([start/3, stop/1, stop/2, cast/2, call/2]).

-include("genesis.hrl").

-record(pdata, {
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

start(Module, Conf, Mods) ->
  Id = Module:id(),
  {ok, _PID} = gen_server:start({global, {Module, Id}}, exch, [Module, Id, Conf, Mods], []).

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

  Data = #pdata{
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

handle_cast(Msg, Data = #pdata{stack = Stack, ctx = Ctx, state = State}) ->
  {Mod, _} = hd(Stack),
  advance(Mod:State(Msg, Ctx), Msg, Data).

handle_call({pdata, state}, _From, Data) ->
  {reply, Data#pdata.state, Data};
handle_call(ctx, _From, Data) ->
  {reply, Data#pdata.ctx, Data};
handle_call(kill, _From, Data) ->
  {stop, normal, ok, Data};
handle_call(Msg, _From, Data = #pdata{module = Module, ctx = Context}) ->
  {ok, Result, NewContext} = Module:call(Msg, Context),
  {reply, Result, Data#pdata{ctx = NewContext}}.

terminate(normal, #pdata{module = Module, ctx = Ctx}) ->
  Module:stop(Ctx);
terminate(_, #pdata{module = Module, ctx = Ctx}) ->
  Module:stop(Ctx).

handle_info(Msg, Data) ->
  handle_cast(Msg, Data).

code_change(_OldVsn, Data, _Extra) ->
  {ok, Data}.

%%%
%%% private
%%%

init(Msg, Data = #pdata{ stack = [{Mod, Params}|_], ctx = Ctx }) ->
  advance(Mod:start(Params, Ctx), Msg, Data#pdata{ state = ?UNDEF }).

advance({continue, Data, Ctx}, _Msg, Data = #pdata{}) ->
  {noreply, Data#pdata{ ctx = Ctx }};

advance({next, State, Ctx}, _Msg, Data) ->
  {noreply, Data#pdata{ state = State, ctx = Ctx }};

advance({skip, Ctx}, Msg, Data = #pdata{module = Module}) ->
  {noreply, Data#pdata{ ctx = Module:dispatch(Msg, Ctx) }};

advance({stop, Ctx}, _Msg, Data = #pdata{ stack = [_] }) ->
  ?LOG([{exch, stack_empty}]),
  {stop, normal, Data#pdata{ ctx = Ctx, stack = [] }};

advance({stop, Ctx}, Msg, Data = #pdata{ stack = [_|T] }) ->
  init(Msg, Data#pdata{ ctx = Ctx, stack = T });

advance({repeat, Ctx}, Msg, Data = #pdata{}) ->
  init(Msg, Data#pdata{ ctx = Ctx });

advance({goto, top, Ctx}, Msg, Data = #pdata{mods = Mods}) ->
  init(Msg, Data#pdata{ ctx = Ctx, stack = Mods});

advance({goto, Mod, Ctx}, Msg, Data = #pdata{stack = Stack}) ->
  init(Msg, Data#pdata{ ctx = Ctx, stack = trim_stack(Mod, Stack) });

advance(Command, Msg, Data) ->
  ?LOG([{command, Command}, {msg, Msg}, {mod, hd(Data#pdata.mods)}, {state, Data#pdata.state}, {pdata, Data}]),
  {noreply, none}.

trim_stack(_Mod, L = [{_LastMod, _}]) -> L;
trim_stack(Mod, L = [{H, _}|_]) when Mod == H -> L;
trim_stack(Mod, [_|T]) -> trim_stack(Mod, T).
