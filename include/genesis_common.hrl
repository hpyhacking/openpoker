-define(UNDEF, undefined).
-define(WAIT_TABLE, 10 * 1000).
-define(CONNECT_TIMEOUT, 2000).
-define(SLEEP, timer:sleep(200)).

%%% Error codes

-define(LOG(L), error_logger:info_report([{debug, {?MODULE, ?LINE, self()}}] ++ L)).
-define(ERROR(L), error_logger:error_report([{debug, {?MODULE, ?LINE, self()}}] ++ L)).

-define(LOOKUP_GAME(Id), global:whereis_name({game, Id})).
-define(PLAYER(Id), {global, {player, Id}}).
-define(LOOKUP_PLAYER(Id), global:whereis_name({player, Id})).

-define(DEF_PWD, "def_pwd").
-define(DEF_HASH_PWD, erlang:phash2(?DEF_PWD, 1 bsl 32)).

-record(exch, {
    id,
    module,
    state,
    mods,
    stack,
    ctx,
    conf
  }).
