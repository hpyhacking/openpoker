-module(op_common).
-compile([export_all]).

get_env(App, Key, Def) ->
  case application:get_env(App, Key) of
    undefined -> Def;
    {ok, Value} -> Value
  end.

get_env(Key, Def) ->
  get_env(genesis, Key, Def).
