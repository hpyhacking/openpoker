-module(op_common).
-compile([export_all]).

get_env(App, Key, Def) ->
  case application:get_env(App, Key) of
    undefined -> Def;
    {ok, Value} -> Value
  end.

get_env(Key, Def) ->
  get_env(genesis, Key, Def).

get_status(NameOrProc) ->
  {status, _Proc, _Mod, SItem} = sys:get_status(NameOrProc),
  [_PDict, _SysStaqte, _Parent, _Dbg, Misc] = SItem,
  [_H|[[{"State", State}]|[]]] = proplists:get_all_values(data, Misc),
  State.
