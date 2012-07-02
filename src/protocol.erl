-module(protocol).
-export([read/1, write/1]).
-export([loop/2]).
-export([id_to_player/1, id_to_game/1]).

-import(pickle, [
    pickle/2, unpickle/2, wrap/2, tuple/1, record/2, 
    byte/0, short/0, int/0, list/2, binary/1, string/0]).

-include("genesis.hrl").

-define(int, int()).
-define(byte, byte()).
-define(string, string()).

nick() -> ?string.
name() -> ?string.
photo() -> ?string.
identity() -> ?string.
password() -> ?string.

state() -> ?int.
game_id() -> ?int.
player_id() -> ?int.

min() -> ?int.
max() -> ?int.
pot() -> ?int.
bet() -> ?int.
call() -> ?int.
raise() -> ?int.
buyin() -> ?int.
inplay() -> ?int.
balance() -> ?int.
amount() -> ?int.

b() -> ?byte.
bb() -> ?byte.
sb() -> ?byte.
sn() -> ?byte.
stage() -> ?byte.
seats() -> ?byte.
joined() -> ?byte.
require() -> ?byte.

suit() -> ?byte.
face() -> ?byte.
rank() -> ?byte.
error() -> ?byte.

high1() -> face().
high2() -> face().

card() -> short().
cards() -> list(byte(), card()).

limit() -> record(limit, {int(), int(), int(), int()}).

game() -> game(get(pass_through)).
player() -> player(get(pass_through)).

loop(connected, ?UNDEF) ->
  {0, 0, 0};

loop(disconnected, {Sum, Good, Bad}) -> 
  ?LOG([{sum, Sum}, {good, Good}, {bad, Bad}]),
  ok;

loop({recv, Bin}, {Sum, Good, Bad}) when is_binary(Bin) ->
  case catch protocol:read(Bin) of
    {'EXIT', {Reason, Stack}} ->
      ?LOG([{recv, Bin}, {error, {Reason, Stack}}]),
      {Sum + 1, Good, Bad + 1};
    R ->
      self() ! {send, list_to_binary(protocol:write(R))},
      {Sum + 1, Good + 1, Bad}
  end.

%%%
%%% private
%%%

internal() -> 
  {fun(Acc, _) -> Acc end, 
    fun(Bin) -> {undefined, Bin} end}.

game(true) -> game_id();
game(_) -> wrap({fun game_to_id/1, fun id_to_game/1}, int()).

player(true) -> player_id();
player(_) -> wrap({fun player_to_id/1, fun id_to_player/1}, int()).

game_to_id(G) when is_pid(G) -> erlang:process_display(self(), backtrace);
game_to_id(GID) when is_integer(GID) -> GID.

id_to_game(0) -> undefined;
id_to_game(GID) -> global:whereis_name({game, GID}).

player_to_id(undefined) -> 0;
player_to_id(none) -> 0;
player_to_id(PID) when is_integer(PID) -> PID.

id_to_player(0) -> undefined;
id_to_player(PID) -> global:whereis_name({player, PID}).

%% auto generate command and notify protocol
%% read binary to protocol record
read(<<?CMD_LOGIN, Bin/binary>>) ->
  unpickle(record(cmd_login, {identity(), password()}), Bin);

read(<<?CMD_LOGOUT, Bin/binary>>) ->
  unpickle(record(cmd_logout, {}), Bin);

read(<<?CMD_QUERY_PLAYER, Bin/binary>>) ->
  unpickle(record(cmd_query_player, {player()}), Bin);

read(<<?CMD_QUERY_BALANCE, Bin/binary>>) ->
  unpickle(record(cmd_query_balance, {}), Bin);

read(<<?CMD_QUERY_GAME, Bin/binary>>) ->
  unpickle(record(cmd_query_game, {}), Bin);

read(<<?CMD_WATCH, Bin/binary>>) ->
  unpickle(record(cmd_watch, {game(), internal(), internal()}), Bin);

read(<<?CMD_UNWATCH, Bin/binary>>) ->
  unpickle(record(cmd_unwatch, {game(), internal(), internal()}), Bin);

read(<<?CMD_JOIN, Bin/binary>>) ->
  unpickle(record(cmd_join, {game(), sn(), buyin(), internal(), internal(), internal(), internal(), internal(), internal()}), Bin);

read(<<?CMD_LEAVE, Bin/binary>>) ->
  unpickle(record(cmd_leave, {game(), internal(), internal(), internal()}), Bin);

read(<<?CMD_RAISE, Bin/binary>>) ->
  unpickle(record(cmd_raise, {game(), amount(), internal(), internal(), internal()}), Bin);

read(<<?CMD_FOLD, Bin/binary>>) ->
  unpickle(record(cmd_fold, {game(), internal(), internal()}), Bin);

read(<<?CMD_QUERY_SEATS, Bin/binary>>) ->
  unpickle(record(cmd_query_seats, {game()}), Bin);

read(<<?CMD_OUT, Bin/binary>>) ->
  unpickle(record(cmd_out, {game(), sn(), buyin(), internal(), internal(), internal()}), Bin);

read(<<?NOTIFY_ACOUNT, Bin/binary>>) ->
  unpickle(record(notify_acount, {balance(), inplay()}), Bin);

read(<<?NOTIFY_GAME, Bin/binary>>) ->
  unpickle(record(notify_game, {game_id(), name(), limit(), seats(), require(), joined()}), Bin);

read(<<?NOTIFY_GAME_DETAIL, Bin/binary>>) ->
  unpickle(record(notify_game_detail, {game_id(), pot(), stage(), limit(), seats(), require(), joined()}), Bin);

read(<<?NOTIFY_GAME_START, Bin/binary>>) ->
  unpickle(record(notify_game_start, {game_id()}), Bin);

read(<<?NOTIFY_GAME_END, Bin/binary>>) ->
  unpickle(record(notify_game_end, {game_id()}), Bin);

read(<<?NOTIFY_GAME_CANCEL, Bin/binary>>) ->
  unpickle(record(notify_game_cancel, {game_id()}), Bin);

read(<<?NOTIFY_STAGE, Bin/binary>>) ->
  unpickle(record(notify_stage, {game_id(), stage()}), Bin);

read(<<?NOTIFY_STAGE_END, Bin/binary>>) ->
  unpickle(record(notify_stage_end, {game_id(), stage()}), Bin);

read(<<?NOTIFY_JOIN, Bin/binary>>) ->
  unpickle(record(notify_join, {game_id(), player_id(), sn(), buyin(), nick(), photo(), internal(), internal()}), Bin);

read(<<?NOTIFY_LEAVE, Bin/binary>>) ->
  unpickle(record(notify_leave, {game_id(), sn(), player_id(), internal(), internal()}), Bin);

read(<<?NOTIFY_BUTTON, Bin/binary>>) ->
  unpickle(record(notify_button, {game_id(), b()}), Bin);

read(<<?NOTIFY_SB, Bin/binary>>) ->
  unpickle(record(notify_sb, {game_id(), sb()}), Bin);

read(<<?NOTIFY_BB, Bin/binary>>) ->
  unpickle(record(notify_bb, {game_id(), bb()}), Bin);

read(<<?NOTIFY_RAISE, Bin/binary>>) ->
  unpickle(record(notify_raise, {game_id(), player_id(), raise(), call()}), Bin);

read(<<?NOTIFY_SEAT, Bin/binary>>) ->
  unpickle(record(notify_seat, {game_id(), sn(), state(), player_id(), inplay(), bet(), nick(), photo()}), Bin);

read(<<?NOTIFY_ACTOR, Bin/binary>>) ->
  unpickle(record(notify_actor, {game_id(), sn()}), Bin);

read(<<?NOTIFY_BETTING, Bin/binary>>) ->
  unpickle(record(notify_betting, {game_id(), call(), min(), max()}), Bin);

read(<<?NOTIFY_DRAW, Bin/binary>>) ->
  unpickle(record(notify_draw, {game_id(), player_id(), card()}), Bin);

read(<<?NOTIFY_PRIVATE, Bin/binary>>) ->
  unpickle(record(notify_private, {game_id(), player_id(), card()}), Bin);

read(<<?NOTIFY_SHARED, Bin/binary>>) ->
  unpickle(record(notify_shared, {game_id(), card()}), Bin);

read(<<?NOTIFY_HAND, Bin/binary>>) ->
  unpickle(record(notify_hand, {game_id(), player_id(), rank(), high1(), high2(), suit()}), Bin);

read(<<?NOTIFY_CARDS, Bin/binary>>) ->
  unpickle(record(notify_cards, {game_id(), player_id(), cards()}), Bin);

read(<<?NOTIFY_WIN, Bin/binary>>) ->
  unpickle(record(notify_win, {game_id(), player_id(), amount()}), Bin);

read(<<?NOTIFY_PLAYER, Bin/binary>>) ->
  unpickle(record(notify_player, {player_id(), nick(), photo()}), Bin);

read(<<?NOTIFY_FOLD, Bin/binary>>) ->
  unpickle(record(notify_fold, {game_id(), sn()}), Bin);

read(<<?NOTIFY_OUT, Bin/binary>>) ->
  unpickle(record(notify_out, {game_id(), player_id()}), Bin);

read(<<?NOTIFY_ERROR, Bin/binary>>) ->
  unpickle(record(notify_error, {error()}), Bin);

read(_ErrorBin) -> ok.

%% write protocol record to binary
write(R) when is_record(R, cmd_login) ->
  [?CMD_LOGIN | pickle(record(cmd_login, {identity(), password()}), R)];

write(R) when is_record(R, cmd_logout) ->
  [?CMD_LOGOUT | pickle(record(cmd_logout, {}), R)];

write(R) when is_record(R, cmd_query_player) ->
  [?CMD_QUERY_PLAYER | pickle(record(cmd_query_player, {player()}), R)];

write(R) when is_record(R, cmd_query_balance) ->
  [?CMD_QUERY_BALANCE | pickle(record(cmd_query_balance, {}), R)];

write(R) when is_record(R, cmd_query_game) ->
  [?CMD_QUERY_GAME | pickle(record(cmd_query_game, {}), R)];

write(R) when is_record(R, cmd_watch) ->
  [?CMD_WATCH | pickle(record(cmd_watch, {game(), internal(), internal()}), R)];

write(R) when is_record(R, cmd_unwatch) ->
  [?CMD_UNWATCH | pickle(record(cmd_unwatch, {game(), internal(), internal()}), R)];

write(R) when is_record(R, cmd_join) ->
  [?CMD_JOIN | pickle(record(cmd_join, {game(), sn(), buyin(), internal(), internal(), internal(), internal(), internal(), internal()}), R)];

write(R) when is_record(R, cmd_leave) ->
  [?CMD_LEAVE | pickle(record(cmd_leave, {game(), internal(), internal(), internal()}), R)];

write(R) when is_record(R, cmd_raise) ->
  [?CMD_RAISE | pickle(record(cmd_raise, {game(), amount(), internal(), internal(), internal()}), R)];

write(R) when is_record(R, cmd_fold) ->
  [?CMD_FOLD | pickle(record(cmd_fold, {game(), internal(), internal()}), R)];

write(R) when is_record(R, cmd_query_seats) ->
  [?CMD_QUERY_SEATS | pickle(record(cmd_query_seats, {game()}), R)];

write(R) when is_record(R, cmd_out) ->
  [?CMD_OUT | pickle(record(cmd_out, {game(), sn(), buyin(), internal(), internal(), internal()}), R)];

write(R) when is_record(R, notify_acount) ->
  [?NOTIFY_ACOUNT | pickle(record(notify_acount, {balance(), inplay()}), R)];

write(R) when is_record(R, notify_game) ->
  [?NOTIFY_GAME | pickle(record(notify_game, {game_id(), name(), limit(), seats(), require(), joined()}), R)];

write(R) when is_record(R, notify_game_detail) ->
  [?NOTIFY_GAME_DETAIL | pickle(record(notify_game_detail, {game_id(), pot(), stage(), limit(), seats(), require(), joined()}), R)];

write(R) when is_record(R, notify_game_start) ->
  [?NOTIFY_GAME_START | pickle(record(notify_game_start, {game_id()}), R)];

write(R) when is_record(R, notify_game_end) ->
  [?NOTIFY_GAME_END | pickle(record(notify_game_end, {game_id()}), R)];

write(R) when is_record(R, notify_game_cancel) ->
  [?NOTIFY_GAME_CANCEL | pickle(record(notify_game_cancel, {game_id()}), R)];

write(R) when is_record(R, notify_stage) ->
  [?NOTIFY_STAGE | pickle(record(notify_stage, {game_id(), stage()}), R)];

write(R) when is_record(R, notify_stage_end) ->
  [?NOTIFY_STAGE_END | pickle(record(notify_stage_end, {game_id(), stage()}), R)];

write(R) when is_record(R, notify_join) ->
  [?NOTIFY_JOIN | pickle(record(notify_join, {game_id(), player_id(), sn(), buyin(), nick(), photo(), internal(), internal()}), R)];

write(R) when is_record(R, notify_leave) ->
  [?NOTIFY_LEAVE | pickle(record(notify_leave, {game_id(), sn(), player_id(), internal(), internal()}), R)];

write(R) when is_record(R, notify_button) ->
  [?NOTIFY_BUTTON | pickle(record(notify_button, {game_id(), b()}), R)];

write(R) when is_record(R, notify_sb) ->
  [?NOTIFY_SB | pickle(record(notify_sb, {game_id(), sb()}), R)];

write(R) when is_record(R, notify_bb) ->
  [?NOTIFY_BB | pickle(record(notify_bb, {game_id(), bb()}), R)];

write(R) when is_record(R, notify_raise) ->
  [?NOTIFY_RAISE | pickle(record(notify_raise, {game_id(), player_id(), raise(), call()}), R)];

write(R) when is_record(R, notify_seat) ->
  [?NOTIFY_SEAT | pickle(record(notify_seat, {game_id(), sn(), state(), player_id(), inplay(), bet(), nick(), photo()}), R)];

write(R) when is_record(R, notify_actor) ->
  [?NOTIFY_ACTOR | pickle(record(notify_actor, {game_id(), sn()}), R)];

write(R) when is_record(R, notify_betting) ->
  [?NOTIFY_BETTING | pickle(record(notify_betting, {game_id(), call(), min(), max()}), R)];

write(R) when is_record(R, notify_draw) ->
  [?NOTIFY_DRAW | pickle(record(notify_draw, {game_id(), player_id(), card()}), R)];

write(R) when is_record(R, notify_private) ->
  [?NOTIFY_PRIVATE | pickle(record(notify_private, {game_id(), player_id(), card()}), R)];

write(R) when is_record(R, notify_shared) ->
  [?NOTIFY_SHARED | pickle(record(notify_shared, {game_id(), card()}), R)];

write(R) when is_record(R, notify_hand) ->
  [?NOTIFY_HAND | pickle(record(notify_hand, {game_id(), player_id(), rank(), high1(), high2(), suit()}), R)];

write(R) when is_record(R, notify_cards) ->
  [?NOTIFY_CARDS | pickle(record(notify_cards, {game_id(), player_id(), cards()}), R)];

write(R) when is_record(R, notify_win) ->
  [?NOTIFY_WIN | pickle(record(notify_win, {game_id(), player_id(), amount()}), R)];

write(R) when is_record(R, notify_player) ->
  [?NOTIFY_PLAYER | pickle(record(notify_player, {player_id(), nick(), photo()}), R)];

write(R) when is_record(R, notify_fold) ->
  [?NOTIFY_FOLD | pickle(record(notify_fold, {game_id(), sn()}), R)];

write(R) when is_record(R, notify_out) ->
  [?NOTIFY_OUT | pickle(record(notify_out, {game_id(), player_id()}), R)];

write(R) when is_record(R, notify_error) ->
  [?NOTIFY_ERROR | pickle(record(notify_error, {error()}), R)];

write(_ErrorRecord) -> ok.
