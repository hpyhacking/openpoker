-module(texas_seat_test).
-compile([export_all]).

-include("genesis_common.hrl").
-include("genesis_game.hrl").
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  R = seat:new(4),
  ?assertEqual(4, size(R)),
  First = element(1, R),
  Last = element(4, R),
  ?assertEqual(1, First#seat.sn),
  ?assertEqual(4, Last#seat.sn).

set_test() ->
  R = seat:set(#seat{sn = 3}, ?PS_PLAY, seat:new(4)),
  [R1] = seat:lookup(?PS_PLAY, R),
  ?assertEqual(?PS_PLAY, R1#seat.state).

get_test() ->
  All = seat:new(5),
  ?assertEqual(?UNDEF, seat:get(0, All)),
  ?assertEqual(?UNDEF, seat:get(6, All)),
  ?assertMatch(#seat{sn = 5, state = ?PS_EMPTY}, seat:get(5, All)).

lookup_test() ->
  All = seat:new(5),
  R = seat:lookup(?PS_EMPTY, All),
  ?assertEqual(5, length(R)),
  First = lists:nth(1, R),
  Last = lists:nth(5, R),
  ?assertEqual(1, First#seat.sn),
  ?assertEqual(?PS_EMPTY, First#seat.state),
  ?assertEqual(5, Last#seat.sn),
  ?assertEqual(?PS_EMPTY, Last#seat.state).

lookup_at_test() ->
  Seats = seat:set([#seat{sn = 2}, #seat{sn = 1}], ?PS_PLAY, seat:new(5)),
  ?assertMatch(#seat{state = ?PS_PLAY, sn = 1}, seat:get(1, Seats)),
  ?assertMatch(#seat{state = ?PS_PLAY, sn = 2}, seat:get(2, Seats)),
  ?assertMatch(#seat{state = ?PS_EMPTY, sn = 3}, seat:get(3, Seats)),
  ?assertMatch(#seat{state = ?PS_EMPTY, sn = 4}, seat:get(4, Seats)),
  ?assertMatch(#seat{state = ?PS_EMPTY, sn = 5}, seat:get(5, Seats)),
  LookupSeats = seat:lookup(?PS_PLAY, Seats, #seat{sn = 1}),
  ?assertMatch([#seat{state = ?PS_PLAY, sn = 2}, #seat{state = ?PS_PLAY, sn = 1}], LookupSeats).

lookup_mask_test() ->
  S = #seat{sn = 3, state = ?PS_PLAY},
  
  R = seat:lookup(?PS_EMPTY, seat:set(S, seat:new(5))),
  ?assertEqual(4, length(R)),
  ?assertEqual(4, (lists:nth(3, R))#seat.sn),

  R1 = seat:lookup(?PS_STANDING, seat:set(S, seat:new(5))),
  ?assertEqual(1, length(R1)),
  ?assertEqual(3, (lists:nth(1, R1))#seat.sn).

set_list_test() ->
  Seats = seat:set([#seat{sn = 2}, #seat{sn = 4}], ?PS_PLAY, seat:new(4)),
  ?assertEqual(#seat{state = ?PS_PLAY, sn = 2}, seat:get(2, Seats)),
  ?assertEqual(#seat{state = ?PS_PLAY, sn = 4}, seat:get(4, Seats)).
