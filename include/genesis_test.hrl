-include_lib("eunit/include/eunit.hrl").

-define(GAME, 1).
-define(GAME_NAME, {global, {game, ?GAME}}).

-define(JACK, jack).
-define(JACK_IDENTITY, "jack").
-define(JACK_ID, 1).
-define(TOMMY, tommy).
-define(TOMMY_IDENTITY, "tommy").
-define(TOMMY_ID, 2).
-define(FOO, foo).
-define(FOO_IDENTITY, "foo").
-define(FOO_ID, 3).

-define(PLAYERS, [{Key, #tab_player_info{pid = Id, identity = Identity, nick = "nick-" ++ Identity, photo = "default", password = ?DEF_HASH_PWD, cash = 0, credit = 1000, disabled = false}} || {Key, Id, Identity} <- [{?JACK, ?JACK_ID, ?JACK_IDENTITY}, {?TOMMY, ?TOMMY_ID, ?TOMMY_IDENTITY}, {?FOO, ?FOO_ID, ?FOO_IDENTITY}]]).

-record(exch, {
    id,
    module,
    state,
    mods,
    stack,
    ctx,
    conf
  }).
