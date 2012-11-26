-module(op_exch_mod).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
    {start, 2}, 
    {dispatch, 2}
  ].
