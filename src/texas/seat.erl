-module(seat).
-export([new/1, set/2, set/3, get/1, get/2, lookup/2, lookup/3, info/2, clear/2]).

-include("openpoker.hrl").

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

set(#seat{sn = SN}, State, Seats) ->
  set(SN, State, Seats);

set(SN, State, Seats) when is_integer(SN) ->
  S = element(SN, Seats),
  setelement(SN, Seats, S#seat{state = State});

set([], _State, Seats) ->
  Seats;

set([#seat{sn = SN}|T], State, Seats) -> 
  set(T, State, set(SN, State, Seats)).

info(size, Seats) ->
  size(Seats).

clear(SN, Seats) ->
  setelement(SN, Seats, #seat{sn = SN, state = ?PS_EMPTY}).

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
