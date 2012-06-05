-module(wait_players).
-export([start/2, wait_for_players/2]).

-include("common.hrl").
-include("protocol.hrl").
-include("game.hrl").

start(_Params, Ctx = #texas{start_delay = StartDelay}) ->
  Timer = erlang:start_timer(StartDelay, self(), ?MODULE),
  {next, wait_for_players, Ctx#texas{ timer = Timer }}.

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{seats = Seats, required = R, joined = J}) when J < R ->
  clear_out_players(seat:lookup(?PS_OUT, Seats), Ctx),
  game:broadcast(#notify_game_cancel{game = Ctx#texas.gid}, Ctx),
  {repeat, Ctx};

wait_for_players({timeout, _, ?MODULE}, Ctx = #texas{seats = Seats}) ->
  ReadySeats = seat:lookup(?PS_READY, Seats),
  case length(ReadySeats) >= Ctx#texas.required of
    true ->
      clear_out_players(seat:lookup(?PS_OUT, Seats), Ctx),
      game:broadcast(#notify_game_start{game = Ctx#texas.gid}, Ctx),
      ReadySeats = seat:lookup(?PS_READY, Seats),
      PlaySeats = seat:set(ReadySeats, ?PS_PLAY, Seats),
      {stop, Ctx#texas{seats = PlaySeats}};
    _ ->
      clear_out_players(seat:lookup(?PS_OUT, Seats), Ctx),
      game:broadcast(#notify_game_cancel{game = Ctx#texas.gid}, Ctx),
      {repeat, Ctx}
  end;

wait_for_players(_R, Ctx) ->
  {skip, Ctx}.

clear_out_players([], _Ctx) -> ok;
clear_out_players([#seat{pid = PId, sn = SN}|T], Ctx) ->
  game:leave(self(), #cmd_leave{game = Ctx#texas.gid, pid = PId, sn = SN}),
  clear_out_players(T, Ctx).
