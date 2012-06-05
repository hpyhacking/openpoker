-module(blinds).
-export([start/2]).

-include("common.hrl").
-include("schema.hrl").
-include("game.hrl").
-include("protocol.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%
%%% callback
%%%

start([], Ctx = #texas{gid = Id, limit = Limit}) ->
  {SmallAmt, BigAmt} = {Limit#limit.small, Limit#limit.big},

  Button = advance_button(Ctx),
  game:broadcast(#notify_button{ game = Id, b = Button#seat.sn }, Ctx),

  {Small, Big, Headsup} = advance_blinds(Button, Ctx),
  game:broadcast(#notify_sb{ game = Id, sb = Small#seat.sn }, Ctx),
  game:broadcast(#notify_bb{ game = Id, bb = Big#seat.sn }, Ctx),

  BlindedCtx = blind([{Small, SmallAmt}, {Big, BigAmt}], Ctx),

  {stop, BlindedCtx#texas{
      sb_amt = SmallAmt, bb_amt = BigAmt,
      b = Button, sb = Small, bb = Big, headsup = Headsup
  }}.

%%
%% private
%%

advance_button(#texas{b = B, seats = Seats}) when B =:= ?UNDEF ->
  [H|_] = seat:lookup(?PS_PLAY, Seats), H;
advance_button(#texas{b = B, seats = Seats}) ->
  [H|_] = seat:lookup(?PS_PLAY, Seats, B), H.

advance_blinds(B, #texas{seats = Seats}) ->
  case seat:lookup(?PS_PLAY, Seats, B) of
    [Hb|[B]] -> % headsup game whit two player (button is small)
      {B, Hb, true};
    [Hs|[Hb|_]] ->
      {Hs, Hb, false}
  end.

blind([], Ctx) -> Ctx;
blind([{Seat = #seat{sn = SN}, Amt}|T], Ctx = #texas{}) ->
  NewCtx = game:bet({Seat, Amt}, Ctx),
  RecoverSeats = seat:set(SN, ?PS_PLAY, NewCtx#texas.seats),
  blind(T, NewCtx#texas{seats = RecoverSeats}).
