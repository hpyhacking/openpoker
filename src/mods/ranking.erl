-module(ranking).
-export([start/2]).
-export([rank/1, notify/1]).

-include("genesis.hrl").

%%%
%%% callback
%%%

start([], Ctx) ->
  notify(Ctx),
  {stop, Ctx}.

%%%
%%% client
%%%

rank(#texas{seats = S, board = Cards}) ->
  Seats = seat:lookup(?PS_STANDING, S),
  rank(Seats, Cards, []).

notify(Ctx) ->
  notify(rank(Ctx), Ctx).

%%% 
%%% private
%%% 

rank([], _Cards, Acc) -> Acc;
rank([H = #seat{hand = Hand}|T], Cards, Acc) ->
  RH = hand:rank(hand:merge(Hand, Cards)),
  rank(T, Cards, [H#seat{hand = RH}|Acc]).

notify([], _Ctx) -> ok;
notify([#seat{pid = PId, process = P, hand = Hand, sn = SN}|T], Ctx = #texas{gid = Id}) ->
  #player_hand{rank = Rank, high1 = H1, high2 = H2, suit = Suit} = hand:player_hand(Hand),
  player:notify(P, #notify_hand{ game = Id, player = PId, sn = SN, rank = Rank, high1 = H1, high2 = H2, suit = Suit}),
  notify(T, Ctx).
