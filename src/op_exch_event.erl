-module(op_exch_event).
-export([start_link/0, add_handler/2, delete_handler/2]).
-export([dispatch/1, cast/1, phantom/1, advance/1, init/2]).

start_link() ->
  gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?MODULE, Handler, Args).

dispatch(Msg) when is_list(Msg) ->
  notify([{event, dispath}] ++ Msg).

cast(Msg) when is_list(Msg) ->
  notify([{event, cast}] ++ Msg).

phantom(Msg) when is_list(Msg) ->
  notify([{event, phantom}] ++ Msg).

advance(Msg) when is_list(Msg) ->
  notify([{event, advance}] ++ Msg).

init(Mod, Msg) when is_atom(Mod) ->
  notify([{event, init}, {Mod, Msg}]).

notify(Msg) ->
  io:format('~n~n================================~n~p~n', [Msg]),
  gen_event:notify(?MODULE, {self(), Msg}).
