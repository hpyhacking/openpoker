-module(schema_test).
-include("genesis.hrl").
-include("genesis_test.hrl").
-export([init/0]).

init_test() ->
  init(),
  [] = mnesia:dirty_read(tab_inplay, 1),
  mnesia:dirty_write(#tab_inplay{pid = 1, inplay = 500}),
  init(),
  [] = mnesia:dirty_read(tab_inplay, 1),
  mnesia:dirty_write(#tab_inplay{pid = 1, inplay = 500}).

init() ->
  ?assertEqual(stopped, mnesia:stop()),
  ?assertEqual(ok, mnesia:delete_schema([node()])),
  ?assertEqual(ok, mnesia:create_schema([node()])),
  ?assertEqual(ok, mnesia:start()),
  ?assertEqual(ok, schema:rebuild_core()).
