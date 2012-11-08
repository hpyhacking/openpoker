-module(genesis_common).
-export([get_status/1]).

get_status(NameOrProc) ->
  {status, _Proc, _Mod, SItem} = sys:get_status(NameOrProc),
  [_PDict, _SysStaqte, _Parent, _Dbg, Misc] = SItem,
  [_H|[[{"State", State}]|[]]] = proplists:get_all_values(data, Misc),
  State.
