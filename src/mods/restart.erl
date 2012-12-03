-module(restart).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2]).

-include("openpoker.hrl").

start([], Ctx) ->
  ResetCtx= Ctx#texas{
    exp_seat = ?UNDEF,
    exp_call = 0,
    exp_min = 0,
    exp_max = 0,
    board = [],
    pot = pot:new(),
    deck = deck:new(),
    seats = reset_seat(seat:get(Ctx#texas.seats), Ctx#texas.seats)
  },

  {goto, top, ResetCtx}.

dispatch(_R, _Ctx) ->
  ok.

reset_seat([], Seats) -> Seats;
reset_seat([H|T], Seats) ->
  reset_seat(T, seat:set(H#seat{hand = hand:new(), bet = 0}, Seats)).
