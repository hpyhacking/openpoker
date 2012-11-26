-module(deal_cards).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2]).

-include("genesis.hrl").

start([0, private], Ctx) -> {stop, Ctx};
start([N, private], Ctx = #texas{b = B, seats = S}) ->
  Seats = seat:lookup(?PS_STANDING, S, B),
  start([N-1, private], draw(Seats, Ctx));

start([0, shared], Ctx) -> {stop, Ctx};
start([N, shared], Ctx) ->
  start([N-1, shared], draw_shared(Ctx)).

dispatch(_R, _Ctx) ->
  ok.

%%%
%%% private
%%%

draw([], Ctx) -> Ctx;
draw([H = #seat{hand = Hand, pid = PId, sn = SN, process = P, identity = Identity}|T], Ctx = #texas{gid = Id, deck = D, seats = S}) ->
  {Card, ND} = deck:draw(D),
  NS = H#seat{ hand = hand:add(Hand, Card) },
  player:notify(P, #notify_private{game = Id, player = PId, sn = SN, card = Card}),
  game:broadcast(#notify_draw{game = Id, player = PId, sn = SN, card = 0}, Ctx, [Identity]),
  draw(T, Ctx#texas{ seats = seat:set(NS, S), deck = ND}).

draw_shared(Ctx = #texas{gid = Id, deck = D, board = B}) ->
  {Card, ND} = deck:draw(D),
  game:broadcast(#notify_shared{ game = Id, card = Card }, Ctx),
  Ctx#texas{ board = [Card|B], deck = ND }.
