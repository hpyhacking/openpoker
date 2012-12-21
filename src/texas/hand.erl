-module(hand).

-export([new/0, new/1, add/2, size/1, rank/1, merge/2]).
-export([make_card/1, make_cards/1, make_rep/1]).
-export([player_hand/1]).

-include("openpoker.hrl").

-define(MASK_A, 2#10000000000000).
-define(MASK_MAX_STRAIGHT, 2#11111000000000).
-define(MASK_MIN_STRAIGHT, 2#00000000011111).

%%%
%%% PUBLIC
%%%

new() -> 
  new([]).

new(Cards) when is_list(Cards) -> 
  #hand{ cards = Cards }.

add(Hand = #hand{}, NewCard) when is_list(NewCard) ->
  add(Hand, make_card(NewCard));
add(Hand = #hand{cards = Cards}, NewCard) when is_integer(NewCard) ->
  Hand#hand{ cards = [NewCard | Cards] }.

size(Hand) ->
  length(Hand#hand.cards).

rank(Hand) ->
  %% 按照花色生成对牌型的位组合
  %% 0b0001 向左位移相应Face的位数，通过对位移后的结果进行与运算
  %% 获取当前花色的手牌组合

  Rep = make_rep(Hand),
  L = [fun is_straight_flush/2,
    fun is_four_kind/2,
    fun is_full_house/2,
    fun is_flush/2,
    fun is_straight/2,
    fun is_three_kind/2,
    fun is_two_pair/2,
    fun is_pair/2],

  rank(Hand, L, Rep).

merge(Hand = #hand{}, []) -> Hand;
merge(Hand = #hand{}, [H|T]) ->
  merge(add(Hand, H), T).

make_cards(S) when is_list(S) ->
  lists:map(fun make_card/1, string:tokens(S, " ")).

make_card([Face, Suit]) ->
  FaceCode = case Face of 
    $2 -> ?CF_TWO;
    $3 -> ?CF_THREE;
    $4 -> ?CF_FOUR;
    $5 -> ?CF_FIVE;
    $6 -> ?CF_SIX;
    $7 -> ?CF_SEVEN;
    $8 -> ?CF_EIGHT;
    $9 -> ?CF_NINE;
    $T -> ?CF_TEN;
    $J -> ?CF_JACK;
    $Q -> ?CF_QUEEN;
    $K -> ?CF_KING;
    $A -> ?CF_ACE
  end,
  SuitCode = case Suit of 
    $C -> ?CS_CLUBS;
    $D -> ?CS_DIAMONDS;
    $H -> ?CS_HEARTS;
    $S -> ?CS_SPADES
  end,
  ?POKER_ENCODE(SuitCode, FaceCode).

make_rep(Hand = #hand{}) ->
  make_rep(Hand#hand.cards);

make_rep(Cards) when is_list(Cards) ->
  make_rep(Cards, {0, 0, 0, 0}).

player_hand(#hand{ rank = Rank, high1 = High3, high2 = High2 }) 
  when Rank == ?HC_FULL_HOUSE;
       Rank == ?HC_TWO_PAIR ->
    H1 = face_from_mask(High3),
    H2 = face_from_mask(High2),
    #player_hand{ rank = Rank, high1 = H1, high2 = H2 };

player_hand(#hand{ rank = Rank, high1 = High3, suit = Suit }) 
  when Rank == ?HC_FLUSH;
       Rank == ?HC_STRAIGHT_FLUSH ->
    H1 = face_from_mask(High3),
    #player_hand{ rank = Rank, high1 = H1, suit = Suit };

player_hand(#hand{ rank = Rank, high1 = High }) ->
    #player_hand{ rank = Rank, high1 = face_from_mask(High) }.

%%%
%%% CHECK RANK
%%%

is_straight_flush(Hand, Rep) ->
    Mask = make_mask(Rep),
    case is_flush(Hand, Mask, Rep, ?CS_CLUBS) of
      none ->
        none;
      Hand1 ->
        High = Hand1#hand.high1,
        case is_straight(Hand, [High, High, High, High]) of
          none ->
            none;
          Hand2 ->
            Hand2#hand{ rank = ?HC_STRAIGHT_FLUSH, suit = Hand1#hand.suit }
      end
    end.

is_flush(Hand, Rep) ->
    Mask = make_mask(Rep),
    is_flush(Hand, Mask, Rep, ?CS_CLUBS).

is_flush(Hand, Mask, [H|T], Suit) ->
  Score = Mask band H,
  Count = bits:bits1(Score),
  if 
    Count < 5 ->
      is_flush(Hand, Mask, T, Suit + 1);
    true ->
      High1 = bits:clear_extra_bits(Score, 5),
      Hand#hand{ rank = ?HC_FLUSH, high1 = High1, suit = Suit }
  end;

is_flush(_, _, [], _) ->
    none.

is_straight(Hand, Rep) ->
  Mask = make_mask(Rep),
  is_straight(Hand, Mask, Rep).

is_straight(Hand, Mask, Rep) when is_list(Rep), Mask band ?MASK_A > 0 ->
  is_straight(Hand, Mask bor 1, ?MASK_MAX_STRAIGHT);
is_straight(Hand, Mask, Rep) when is_list(Rep) ->
  is_straight(Hand, Mask, ?MASK_MAX_STRAIGHT);

is_straight(_, _, Mask) when Mask < ?MASK_MIN_STRAIGHT -> none;
is_straight(Hand, Value, Mask) when Value band Mask =:= Mask ->
  Hand#hand{ rank = ?HC_STRAIGHT, high1 = Mask };
is_straight(Hand, Value, Mask) ->
  is_straight(Hand, Value, Mask bsr 1).
  
is_four_kind(Hand, [C, D, H, S]) when C band D band H band S > 0 ->
  V = C band D band H band S,
  Hand#hand{ 
    rank = ?HC_FOUR_KIND, 
    high1 = V, 
    score = score([C, D, H, S], V, 1)};
is_four_kind(_, _) ->
  none.

is_full_house(Hand, Rep) ->
  case is_three_kind(Hand, Rep) of
    none -> 
      none;
    Hand1 ->
      High3 = Hand1#hand.high1,
      case is_pair(Hand1, clear_high_bit(Rep, High3)) of
        none ->
          none;
        Hand2 ->
          High2 = Hand2#hand.high1,
          Hand2#hand{ 
            rank = ?HC_FULL_HOUSE, 
            high1 = High3,
            high2 = High2,
            score = 0
          }
      end
  end.

is_three_kind(Hand, [C, D, H, S]) ->
  LAND = [C band D band H,
          D band H band S,
          H band S band C,
          S band C band D],
  L = lists:sort(fun(A, B) -> A > B end, LAND),
  is_three_kind(Hand, L, [C, D, H, S]).

is_three_kind(_, [], _) -> none;
is_three_kind(Hand, [H|_], Rep) when H > 0 ->
    Hand#hand{
      rank = ?HC_THREE_KIND,
      high1 = high_bit(H),
      score = score(Rep, H, 2)};
is_three_kind(Hand, [_|T], Rep) ->
  is_three_kind(Hand, T, Rep).

is_two_pair(Hand, Rep) ->
  case is_pair(Hand, Rep) of
    none ->
      none;
    Hand1 = #hand{ rank = ?HC_PAIR, high1 = High1 } ->
      Rep1 = clear_high_bit(Rep, High1),
      case is_pair(Hand1, Rep1) of
        none ->
          none;
        Hand2 = #hand{ rank = ?HC_PAIR, high1 = High2 } ->
          Hand2#hand{ 
            rank = ?HC_TWO_PAIR,
            high1 = High1,
            high2 = High2,
            score = score(Rep, High1 bor High2, 1)
          }
      end
  end.

is_pair(Hand, [C, D, H, S]) ->
  LAND = [C band D,
          D band H,
          H band S,
          S band C,
          C band H,
          D band S],
  L = lists:sort(fun(A, B) -> A > B end, LAND),
  is_pair(Hand, L, [C, D, H, S]).

is_pair(_, [], _) -> none;
is_pair(Hand, [H|_], Rep) when H > 0 ->
  Hand#hand{
    rank = ?HC_PAIR,
    high1 = high_bit(H),
    score = score(Rep, H, 3)};
is_pair(Hand, [_|T], Rep) ->
  is_pair(Hand, T, Rep).


%%%
%%% PRIVATE
%%%

rank(Hand, [Rank|T], Rep) ->
  case Rank(Hand, Rep) of
    none ->
      rank(Hand, T, Rep);
    Hand1 ->
      Hand1
  end;

rank(Hand, [], Rep) ->
  Mask = make_mask(Rep),
  High = bits:clear_extra_bits(Mask, 5),
  Hand#hand{ rank = ?HC_HIGH_CARD, high1 = High, score = 0 }.

make_rep([H|T], Rep) when is_integer(H) -> 
  [Suit, Face] = ?POKER_DECODE(H),
  make_rep(T, setelement(Suit, Rep, element(Suit, Rep) bor (1 bsl (Face))));

make_rep([], Rep) ->
  tuple_to_list(Rep).

make_mask([C, D, H, S]) ->
    C bor D bor H bor S.

high_bit(Mask) ->
    1 bsl bits:log2(Mask).

clear_high_bit([C, D, H, S], High) ->
    [C band (bnot High),
     D band (bnot High),
     H band (bnot High),
     S band (bnot High)].

score(Rep, High, Bits) ->
    Mask = make_mask(Rep),
    Mask1 = Mask band (bnot High),
    bits:clear_extra_bits(Mask1, Bits).

face_from_mask(0) -> 0;
face_from_mask(X) when is_number(X) ->
  L = [?CF_ACE, ?CF_KING, ?CF_QUEEN, ?CF_JACK, 
       ?CF_TEN, ?CF_NINE, ?CF_EIGHT, ?CF_SEVEN,
       ?CF_SIX, ?CF_FIVE, ?CF_FOUR,  ?CF_THREE, ?CF_TWO],
  face_from_mask(X, [1 bsl Face || Face <- L]).

face_from_mask(_, []) -> 0;
face_from_mask(X, [H|_]) when (X band H) > 0 ->
  bits:log2(H);
face_from_mask(X, [_|T]) ->
  face_from_mask(X, T).
