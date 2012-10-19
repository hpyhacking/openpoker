-module(showdown).
-export([start/2]).

-include("genesis.hrl").

%%%
%%% callback
%%%

start([], Ctx = #texas{gid = Id, seats = S, pot = P}) ->
  RankedSeats = ranking:rank(Ctx),
  Winners = winners(RankedSeats, pot:pots(P)),

  show_cards(seat:lookup(?PS_ANY, S), Ctx),
  broadcast_ranks(RankedSeats, Ctx),

  RewardedCtx = reward_winners(Winners, Ctx),
  RewardedSeats = RewardedCtx#texas.seats,
  KickedCtx = kick_poor_players(seat:lookup(?PS_READY, RewardedSeats), RewardedCtx),

  ResetCtx= KickedCtx#texas{
    board = [],
    pot = pot:new(),
    deck = deck:new(),
    seats = reset_seat(seat:get(KickedCtx#texas.seats), KickedCtx#texas.seats)
  },

  game:broadcast(#notify_game_end{ game = Id }, ResetCtx),

  {stop, ResetCtx}.

%%%
%%% private
%%%

reset_seat([], Seats) -> Seats;
reset_seat([H|T], Seats) ->
  reset_seat(T, seat:set(H#seat{hand = hand:new()}, Seats)).


show_cards([], _Ctx) -> ok;
show_cards([#seat{pid = PId, identity = Identity, hand = Hand, sn = SN}|T], Ctx = #texas{gid = Id}) ->
  game:broadcast(#notify_cards{ game = Id, player = PId, sn = SN, cards = Hand#hand.cards}, Ctx, [Identity]),
  show_cards(T, Ctx).

reward_winners([], Ctx) -> Ctx;
reward_winners([{H = #hand{}, Amt}|T], Ctx) -> 
  reward_winners(T, game:reward(H, Amt, Ctx)).

kick_poor_players([], Ctx) -> Ctx;
kick_poor_players([#seat{pid = PId, sn = SN, inplay = Inplay}|T], Ctx = #texas{seats = S, limit = L})
when L#limit.big > Inplay ->
  game:broadcast(#notify_out{ game = Ctx#texas.gid, player = PId }, Ctx),
  kick_poor_players(T, Ctx#texas{seats = seat:set(SN, ?PS_OUT, S)});
kick_poor_players([_|T], Ctx = #texas{}) ->
  kick_poor_players(T, Ctx).
      
broadcast_ranks([], _Ctx) -> ok;
broadcast_ranks([#seat{pid = PId, hand = Hand, sn = SN}|T], Ctx = #texas{gid = Id}) ->
  #player_hand{rank = Rank, high1 = H1, high2 = H2, suit = Suit} = hand:player_hand(Hand),
  game:broadcast(#notify_hand{ player = PId, sn = SN, game = Id, rank = Rank, high1 = H1, high2 = H2, suit = Suit}, Ctx),
  broadcast_ranks(T, Ctx).

%% fuck code is here, winners comput to depend on record field position
%% e.g lists:keysort(5, M) is sort by hand record five point field rank
%% TODO use lists:sort(Fun, List) rework winners function

winners(RankedSeats, Pots) ->
  %% to cope fuck winners, set pid and process every seat hand
  Fun = fun(#seat{pid = PId, sn = SN, hand = Hand}) -> 
      Hand#hand{seat_sn = SN, pid = PId}
  end, 
  FuckedHands = lists:map(Fun, RankedSeats),
  gb_trees:to_list(winners(FuckedHands, Pots, gb_trees:empty())).

winners(_Ranks, [], Winners) -> Winners;
winners(Ranks, [{Total, Members}|Rest], Winners) ->
    M = lists:filter(fun(#hand{pid = PId}) -> gb_trees:is_defined(PId, Members) end, Ranks),
    %% sort by rank and leave top ranks only
    M1 = lists:reverse(lists:keysort(5, M)),
    TopRank = element(5, hd(M1)),
    M2 = lists:filter(fun(R) -> element(5, R) == TopRank end, M1),
    %% sort by high card and leave top high cards only
    M3 = lists:reverse(lists:keysort(6, M2)),
    TopHigh1 = element(6, hd(M3)),
    M4 = lists:filter(fun(R) -> element(6, R) == TopHigh1 end, M3),
    M5 = lists:reverse(lists:keysort(7, M4)),
    TopHigh2 = element(7, hd(M5)),
    M6 = lists:filter(fun(R) -> element(7, R) == TopHigh2 end, M5),
    %% sort by top score and leave top scores only
    M7 = lists:reverse(lists:keysort(8, M6)),
    TopScore = element(8, hd(M7)),
    M8 = lists:filter(fun(R) -> element(8, R) == TopScore end, M7),
    Win = Total div length(M8),
    Winners1 = update_winners(M8, Win, Winners),
    winners(Ranks, Rest, Winners1).

update_winners([], _Amount, Tree) ->
    Tree;
update_winners([Player|Rest], Amount, Tree) ->
  update_winners(Rest, Amount, update_counter(Player, Amount, Tree)).

update_counter(Key, Amount, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Old} ->
            Old = gb_trees:get(Key, Tree),
            gb_trees:update(Key, Old + Amount, Tree);
        none ->
            gb_trees:insert(Key, Amount, Tree)
    end.
