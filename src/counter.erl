-module(counter).

-export([bump/1, bump/2, reset/1, reset/2]).

-include("schema.hrl").

bump(Type) ->
    bump(Type, 1).

bump(Type, Inc) ->
    mnesia:dirty_update_counter(tab_counter, Type, Inc).

reset(Type) ->
  reset(Type, 0).

reset(Type, Count) ->
    Counter = #tab_counter {
      type = Type,
      value = Count
     },
    ok = mnesia:dirty_write(Counter).

