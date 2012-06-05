-module(deal_cards).
-export([start/2]).

-include("common.hrl").
-include("game.hrl").
-include("protocol.hrl").

start([0, private], Ctx) -> {stop, Ctx};
start([N, private], Ctx = #texas{b = B, seats = S}) ->
  Seats = seat:lookup(?PS_STANDING, S, B),
  start([N-1, private], draw(Seats, Ctx));

start([0, shared], Ctx) -> {stop, Ctx};
start([N, shared], Ctx) ->
  start([N-1, shared], draw_shared(Ctx)).

%%%
%%% private
%%%

draw([], Ctx) -> Ctx;
draw([H = #seat{hand = Hand, pid = PId, process = P, identity = Identity}|T], Ctx = #texas{gid = Id, deck = D, seats = S}) ->
  {Card, ND} = deck:draw(D),
  NS = H#seat{ hand = hand:add(Hand, Card) },
  player:notify(P, #notify_private{game = Id, player = PId, card = Card}),
  game:broadcast(#notify_draw{game = Id, player = PId, card = 0}, Ctx, [Identity]),
  draw(T, Ctx#texas{ seats = seat:set(NS, S), deck = ND}).

draw_shared(Ctx = #texas{gid = Id, deck = D, board = B}) ->
  {Card, ND} = deck:draw(D),
  game:broadcast(#notify_shared{ game = Id, card = Card }, Ctx),
  Ctx#texas{ board = [Card|B], deck = ND }.
