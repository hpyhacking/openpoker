-module(seat).
-export([new/1, set/2, set/3, get/1, get/2, lookup/2, lookup/3, info/2]).

-include("common.hrl").
-include("game.hrl").

new(N) when N > 0 ->
  list_to_tuple(new(N, [])).

lookup(Mask, Seats, _At = #seat{sn = SN})  
when SN > 0, SN =< size(Seats) ->
  Size = size(Seats),
  lookup(Mask, Seats, Size, SN, Size, []).

lookup(Mask, Seats) ->
  Size = size(Seats),
  lookup(Mask, Seats, Size, Size, Size, []).

set(Seat = #seat{sn = SN}, Seats) ->
  setelement(SN, Seats, Seat).

get(Seats) ->
  tuple_to_list(Seats).

get(SN, Seats) when is_tuple(Seats), SN >= 1, SN =< size(Seats) -> element(SN, Seats);
get(_SN, _Seats) -> ?UNDEF.

set(#seat{sn = SN}, State, Seats) -> set(SN, State, Seats);
set(SN, State, Seats) when is_integer(SN) ->
  S = element(SN, Seats),
  setelement(SN, Seats, S#seat{state = State});

set([], _State, Seats) -> Seats;
set([#seat{sn = SN}|T], State, Seats) -> 
  set(T, State, set(SN, State, Seats)).

info(size, Seats) ->
  size(Seats).

%%%
%%% private
%%%

new(0, Acc) -> Acc;
new(N, Acc) -> new(N - 1, [#seat{sn = N, state = ?PS_EMPTY} | Acc]).

lookup(_Mask, _Seats, 0, _At, _N, _Acc) -> [];
lookup(_Mask, _Seats, _Size, _At, 0, Acc) -> lists:reverse(Acc);
lookup(Mask, Seats, Size, At, N, Acc) ->
  SN = (At rem Size) + 1,
  R = element(SN, Seats),
  NewAcc = case check(R#seat.state, Mask) of
    true ->
      [R|Acc];
    _ ->
      Acc
  end,
  lookup(Mask, Seats, Size, At + 1, N - 1, NewAcc).

check(?PS_EMPTY, ?PS_EMPTY) -> true;
check(_, ?PS_EMPTY) -> false;
check(?PS_EMPTY, _) -> false;
check(State, Mask) -> 
  (State band Mask) =:= State.

  

%%%
%%% unit test
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  R = new(4),
  ?assertEqual(4, size(R)),
  First = element(1, R),
  Last = element(4, R),
  ?assertEqual(1, First#seat.sn),
  ?assertEqual(4, Last#seat.sn).

set_test() ->
  R = set(#seat{sn = 3}, ?PS_PLAY, new(4)),
  [R1] = seat:lookup(?PS_PLAY, R),
  ?assertEqual(?PS_PLAY, R1#seat.state).

get_test() ->
  All = new(5),
  ?assertEqual(?UNDEF, get(0, All)),
  ?assertEqual(?UNDEF, get(6, All)),
  ?assertMatch(#seat{sn = 5, state = ?PS_EMPTY}, get(5, All)).

lookup_test() ->
  All = new(5),
  R = seat:lookup(?PS_EMPTY, All),
  ?assertEqual(5, length(R)),
  First = lists:nth(1, R),
  Last = lists:nth(5, R),
  ?assertEqual(1, First#seat.sn),
  ?assertEqual(?PS_EMPTY, First#seat.state),
  ?assertEqual(5, Last#seat.sn),
  ?assertEqual(?PS_EMPTY, Last#seat.state).

lookup_at_test() ->
  Seats = set([#seat{sn = 2}, #seat{sn = 1}], ?PS_PLAY, new(5)),
  ?assertMatch(#seat{state = ?PS_PLAY, sn = 1}, get(1, Seats)),
  ?assertMatch(#seat{state = ?PS_PLAY, sn = 2}, get(2, Seats)),
  ?assertMatch(#seat{state = ?PS_EMPTY, sn = 3}, get(3, Seats)),
  ?assertMatch(#seat{state = ?PS_EMPTY, sn = 4}, get(4, Seats)),
  ?assertMatch(#seat{state = ?PS_EMPTY, sn = 5}, get(5, Seats)),
  LookupSeats = seat:lookup(?PS_PLAY, Seats, #seat{sn = 1}),
  ?assertMatch([#seat{state = ?PS_PLAY, sn = 2}, #seat{state = ?PS_PLAY, sn = 1}], LookupSeats).

lookup_mask_test() ->
  S = #seat{sn = 3, state = ?PS_PLAY},
  
  R = seat:lookup(?PS_EMPTY, seat:set(S, new(5))),
  ?assertEqual(4, length(R)),
  ?assertEqual(4, (lists:nth(3, R))#seat.sn),

  R1 = seat:lookup(?PS_STANDING, seat:set(S, new(5))),
  ?assertEqual(1, length(R1)),
  ?assertEqual(3, (lists:nth(1, R1))#seat.sn).

set_list_test() ->
  Seats = set([#seat{sn = 2}, #seat{sn = 4}], ?PS_PLAY, new(4)),
  ?assertEqual(#seat{state = ?PS_PLAY, sn = 2}, get(2, Seats)),
  ?assertEqual(#seat{state = ?PS_PLAY, sn = 4}, get(4, Seats)).
-endif.
