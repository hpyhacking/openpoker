-module(hand).

-export([new/0, new/1, add/2, rank/1, merge/2, size/1]).
-export([make_card/1, make_card/2, make_cards/1, print_bin/1, 
         print_rep/1, to_string/1, player_hand/1, card_to_string/1]).

-include("genesis.hrl").

new() -> new([]).
new(Cards) -> #hand{ cards = Cards }.

add(Hand, Card) ->
  Hand#hand{ cards = [Card|Hand#hand.cards] }.

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

merge(Hand, []) -> Hand;
merge(Hand, [H|T]) ->
  merge(add(Hand, H), T).

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

is_straight(Hand, Mask, Rep) 
  when is_list(Rep) ->
    if             %AKQJT98765432A
  Mask band 2#10000000000000 > 0 ->
      Value = Mask bor 1;
  true ->
      Value = Mask
    end,                %AKQJT98765432A
    is_straight(Hand, Value, 2#11111000000000);

is_straight(_, _, Mask) 
  when Mask < 2#11111 ->
    none;

is_straight(Hand, Value, Mask) 
  when Mask >= 2#11111 ->
    if 
  Value band Mask =:= Mask ->
            Hand#hand{ rank = ?HC_STRAIGHT, high1 = Mask };
  true ->
      is_straight(Hand, Value, Mask bsr 1)
    end.
  
is_four_kind(Hand, [C, D, H, S]) ->
    Value = C band D band H band S,
    if
  Value > 0 ->
            Hand#hand{ 
              rank = ?HC_FOUR_KIND, 
              high1 = Value, 
              score = score([C, D, H, S], Value, 1)
             };
  true ->
      none
    end.

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
    L = lists:sort(fun(A, B) ->
         A > B
       end, [C band D band H,
       D band H band S,
       H band S band C,
       S band C band D]),
    is_three_kind(Hand, L, [C, D, H, S]).

is_three_kind(Hand, [H|T], Rep) ->
    if 
  H > 0 ->
            Hand#hand{
              rank = ?HC_THREE_KIND, 
              high1 = high_bit(H), 
              score = score(Rep, H, 2)
             };
  true ->
      is_three_kind(Hand, T, Rep)
    end;

is_three_kind(_, [], _) ->
    none.

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
    L = lists:sort(fun(A, B) ->
         A > B
       end, [C band D,
       D band H,
       H band S,
       S band C,
       C band H,
       D band S]),
    is_pair(Hand, L, [C, D, H, S]).

is_pair(Hand, [H|T], Rep) ->
    if 
  H > 0 ->
            Hand#hand{ 
              rank = ?HC_PAIR, 
              high1 = high_bit(H), 
              score = score(Rep, H, 3)
             };
  true ->
      is_pair(Hand, T, Rep)
    end;

is_pair(_, [], _) ->
    none.

make_rep(Hand = #hand{}) ->
  make_rep(Hand#hand.cards);

make_rep(Cards) 
  when is_list(Cards) ->
    make_rep(Cards, {0, 0, 0, 0}).

make_rep([H|T], Rep) 
  when is_integer(H) -> 
    Face = 1 bsl (H bsr 8),
    Suit = H band 16#ff,
    Old = element(Suit, Rep),
    make_rep(T, setelement(Suit, Rep, Old bor Face));

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

%% Make a list of cards from a space-delimited string 

make_cards(S)
  when is_list(S) ->
    lists:map(fun make_card/1, 
        string:tokens(S, " ")).

make_card({F, S}) ->
    Face = case F of 
         two -> ?CF_TWO;
         three -> ?CF_THREE;
         four -> ?CF_FOUR;
         five -> ?CF_FIVE;
         six -> ?CF_SIX;
         seven -> ?CF_SEVEN;
         eight -> ?CF_EIGHT;
         nine -> ?CF_NINE;
         ten -> ?CF_TEN;
         jack -> ?CF_JACK;
         queen -> ?CF_QUEEN;
         king -> ?CF_KING;
         ace -> ?CF_ACE
     end,
    Suit = case S of 
         clubs -> ?CS_CLUBS;
         diamonds -> ?CS_DIAMONDS;
         hearts -> ?CS_HEARTS;
         spades -> ?CS_SPADES
     end,
    make_card(Face, Suit);

make_card([H, T]) ->
    Face = case H of 
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
    Suit = case T of 
         $C -> ?CS_CLUBS;
         $D -> ?CS_DIAMONDS;
         $H -> ?CS_HEARTS;
         $S -> ?CS_SPADES
     end,
    make_card(Face, Suit).

make_card(Face, Suit) ->
    (Face bsl 8) bor Suit.

face_from_mask(0) ->
    0;

face_from_mask(X) 
  when is_number(X) ->
    face_from_mask(X, [1 bsl Face || Face <- [?CF_ACE, ?CF_KING, ?CF_QUEEN, 
                                              ?CF_JACK, ?CF_TEN, ?CF_NINE,
                                              ?CF_EIGHT, ?CF_SEVEN, ?CF_SIX, 
                                              ?CF_FIVE, ?CF_FOUR, ?CF_THREE, 
                                              ?CF_TWO]]).

face_from_mask(_, []) ->
    0;

face_from_mask(X, [H|_])
  when (X band H) > 0 ->
    bits:log2(H);

face_from_mask(X, [_|T]) ->
    face_from_mask(X, T).

face_to_string(Face) 
  when is_integer(Face) ->
    case Face of
        ?CF_TWO -> "two";
        ?CF_THREE -> "three";
        ?CF_FOUR -> "four";
        ?CF_FIVE -> "five";
        ?CF_SIX -> "six";
        ?CF_SEVEN -> "seven";
        ?CF_EIGHT -> "eight";
        ?CF_NINE -> "nine";
        ?CF_TEN -> "ten";
        ?CF_JACK -> "jack";
        ?CF_QUEEN -> "queen";
        ?CF_KING -> "king";
        ?CF_ACE -> "ace"
    end.

suit_to_string(Suit)
  when is_integer(Suit) ->
    case Suit of 
        ?CS_CLUBS -> "clubs";
        ?CS_DIAMONDS -> "diamonds";
        ?CS_HEARTS -> "hearts";
        ?CS_SPADES -> "spades"
    end.

card_to_string(Card) ->
    Face = Card bsr 8,
    Suit = Card band 16#ff,
    face_to_string(Face) ++ " of " ++ suit_to_string(Suit).
    
to_string(H = #player_hand{ rank = ?HC_STRAIGHT_FLUSH }) ->
    "straight flush high " 
  ++ face_to_string(H#player_hand.high1)
  ++ "s";

to_string(H = #player_hand{ rank = ?HC_FOUR_KIND }) ->
    "four of a kind " 
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_FULL_HOUSE }) ->
    "house of " 
  ++ face_to_string(H#player_hand.high1) 
  ++ "s full of " 
  ++ face_to_string(H#player_hand.high2)
  ++ "s";

to_string(H = #player_hand{ rank = ?HC_FLUSH }) ->
    "flush high "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_STRAIGHT }) ->
    "straight high "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_THREE_KIND }) ->
    "three of a kind "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_TWO_PAIR }) ->
    "two pairs of "
  ++ face_to_string(H#player_hand.high1)
  ++ "s and "
  ++ face_to_string(H#player_hand.high2)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_PAIR }) ->
    "pair of "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_HIGH_CARD }) ->
    "high card "
  ++ face_to_string(H#player_hand.high1).

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

print_bin(X) ->
    io:format("AKQJT98765432A~n"),
    io:format("~14.2.0B~n", [X]).

print_rep([C, D, H, S]) ->
    print_rep({C, D, H, S});

print_rep({C, D, H, S}) ->
    io:format("   AKQJT98765432A~n"),
    io:format("C: ~14.2.0B~n", [C]),
    io:format("D: ~14.2.0B~n", [D]),
    io:format("H: ~14.2.0B~n", [H]),
    io:format("S: ~14.2.0B~n", [S]).

%%%
%%% Test suite
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_rep_test() ->
    %%  AKQJT98765432A
    [2#00000010000000,
     2#00101000011000,
     2#00010001000000,
     2#00000000000000]
  = make_rep(make_cards("4D JH 5D 8C QD TD 7H")).

rank_test_hand(Cards) ->
    Hand = new(make_cards(Cards)),
    rank(Hand).

rank_test_player_hand(Cards) ->
  player_hand(rank_test_hand(Cards)).

short(Hand) ->
    {Hand#hand.rank, 
     Hand#hand.high1, 
     Hand#hand.high2,
     Hand#hand.score}.
    
rank_high_card1_test() ->
    H = rank_test_hand("4D JH 5D 8C QD TD 7H"),
    ?assertEqual(?HC_HIGH_CARD, H#hand.rank),
    ?assertEqual(2#00111011000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_high_card2_test() ->
    H = rank_test_hand("8C AD 5H 3S KD 9D 4D"),
    ?assertEqual(?HC_HIGH_CARD, H#hand.rank),
    ?assertEqual(2#11000110010000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_high_card3_test() ->
    H = rank_test_hand("4C JH 5C 8D QC 2C 3D"),
    ?assertEqual(?HC_HIGH_CARD, H#hand.rank),
    ?assertEqual(2#00110010011000, H#hand.high1), 
    ?assertEqual(0, H#hand.score).
    
rank_pair1_test() ->
    H = rank_test_hand("KD 3S 5H 3D 6C QH 9S"),
    ?assertEqual(?HC_PAIR, H#hand.rank),
    ?assertEqual(2#00000000000100, H#hand.high1),
    ?assertEqual(2#01100100000000, H#hand.score).

rank_pair2_test() ->
    H = rank_test_hand("AC 2D 5D AS 4H 9D KD"),
    ?assertEqual(?HC_PAIR, H#hand.rank),
    ?assertEqual(2#10000000000000, H#hand.high1),
    ?assertEqual(2#01000100010000, H#hand.score).

rank_pair3_test() ->
    H = rank_test_hand("9S JH 5D TS 3C KC 3H"),
    ?assertEqual(?HC_PAIR, H#hand.rank),
    ?assertEqual(2#00000000000100, H#hand.high1),
    ?assertEqual(2#01011000000000, H#hand.score).

rank_two_pair1_test() ->
    H = rank_test_hand("QC KD JD QD JC 5C KC"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#01000000000000, H#hand.high1),
    ?assertEqual(2#00100000000000, H#hand.high2),
    ?assertEqual(2#00010000000000, H#hand.score).

rank_two_pair2_test() ->
    H = rank_test_hand("7H 3H 6C TD 7C JH 6H"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#00000001000000, H#hand.high1),
    ?assertEqual(2#00000000100000, H#hand.high2),
    ?assertEqual(2#00010000000000, H#hand.score).

rank_two_pair3_test() ->
    H = rank_test_hand("4D 3S 5H JD JC QH 5S"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#00000000010000, H#hand.high2),
    ?assertEqual(2#00100000000000, H#hand.score).

rank_two_pair4_test() ->
    H = rank_test_hand("AC 2D 5D AS 5H 9D 4D"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#10000000000000, H#hand.high1),
    ?assertEqual(2#00000000010000, H#hand.high2),
    ?assertEqual(2#00000100000000, H#hand.score).

rank_two_pair5_test() ->
    H = rank_test_hand("9S JH 5D JS 5C KC 3D"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#00000000010000, H#hand.high2),
    ?assertEqual(2#01000000000000, H#hand.score).

rank_three_kind1_test() ->
    H = rank_test_hand("KH 9S 5H QD QC QH 3S"),
    ?assertEqual(?HC_THREE_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#01000100000000, H#hand.score).

rank_three_kind2_test() ->
    H = rank_test_hand("AC KC KD KS 7H 9D 4D"),
    ?assertEqual(?HC_THREE_KIND, H#hand.rank),
    ?assertEqual(2#01000000000000, H#hand.high1),
    ?assertEqual(2#10000100000000, H#hand.score).

rank_three_kind3_test() ->
    H = rank_test_hand("KS TS QD QS QH 4C 5D"),
    ?assertEqual(?HC_THREE_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#01001000000000, H#hand.score).

rank_straight1_test() ->
    H = rank_test_hand("KC QS JH TC 9C 4D 3S"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight2_test() ->
    H = rank_test_hand("AC KS QH JC TC 9D 4D"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#11111000000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight3_test() ->
    H = rank_test_hand("KS QD JS TC 9S 2D 7S"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight4_test() ->
    H = rank_test_hand("5C 4D 3H 2C AD 7H 9S"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#00000000011111, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight5_test() ->
    H = rank_test_hand("5H 4S JC 8S 7D 6C 3C"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#00000011111000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_flush1_test() ->
    H = rank_test_hand("4D JD 5D JC QD 2D 7H"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#00110000011010, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_flush2_test() ->
    H = rank_test_hand("8C AD 5D AS KD 9D 4D"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#11000100011000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_flush3_test() ->
    H = rank_test_hand("4C JC 5C 8D QC 3C 7S"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#00110000011100, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_full_house1_test() ->
    H = rank_test_hand("4D JS 5H JD JC QH QS"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#00100000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house2_test() ->
    H = rank_test_hand("AC AD KD AS KH 9D 4D"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#10000000000000, H#hand.high1),
    ?assertEqual(2#01000000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house3_test() ->
    H = rank_test_hand("3S JH JD JS KH KC 5D"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#01000000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house4_test() ->
    H = rank_test_hand("TD QH TH TC 6C QD QC"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#00001000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_four_kind1_test() ->
    H = rank_test_hand("4D AS 5H QD QC QH QS"),
    ?assertEqual(?HC_FOUR_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#10000000000000, H#hand.score).

rank_four_kind2_test() ->
    H = rank_test_hand("AC KC KD KS KH 9D 4D"),
    ?assertEqual(?HC_FOUR_KIND, H#hand.rank),
    ?assertEqual(2#01000000000000, H#hand.high1),
    ?assertEqual(2#10000000000000, H#hand.score).

rank_four_kind3_test() ->
    H = rank_test_hand("KS TS QD QS QH QC 5D"),
    ?assertEqual(?HC_FOUR_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#01000000000000, H#hand.score).

rank_straight_flush1_test() ->
    H = rank_test_hand("KC QC JC TC 9C 4D AS"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight_flush2_test() ->
    H = rank_test_hand("AC KC QC JC TC 9D 4D"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#11111000000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight_flush3_test() ->
    H = rank_test_hand("KS QS JS TS 9S AD 7S"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

high_card_win_test() ->
    H1 = rank_test_hand("4D JH 5D 8C QD TD 7H"),
    H2 = rank_test_hand("8C AD 5H 3S KD 9D 4D"),
    H3 = rank_test_hand("4C JH 5C 8D QC 2C 3D"),
    ?assertEqual(?HC_HIGH_CARD, H1#hand.rank),
    ?assertEqual(?HC_HIGH_CARD, H2#hand.rank),
    ?assertEqual(?HC_HIGH_CARD, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H1) > short(H3)).

pair_win_test() ->
    H1 = rank_test_hand("KD 3S 5H 3D 6C QH 9S"),
    H2 = rank_test_hand("AC 2D 5D AS 4H 9D KD"),
    H3 = rank_test_hand("9S JH 5D TS 3C KC 3H"),
    ?assertEqual(?HC_PAIR, H1#hand.rank),
    ?assertEqual(?HC_PAIR, H2#hand.rank),
    ?assertEqual(?HC_PAIR, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H1) > short(H3)).

two_pair_win_test() ->
    H1 = rank_test_hand("4D 3S 5H JD JC QH 5S"),
    H2 = rank_test_hand("AC 2D 5D AS 5H 9D 4D"),
    H3 = rank_test_hand("9S JH 5D JS 5C KC 3D"),
    ?assertEqual(?HC_TWO_PAIR, H1#hand.rank),
    ?assertEqual(?HC_TWO_PAIR, H2#hand.rank),
    ?assertEqual(?HC_TWO_PAIR, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H3) > short(H1)).

three_kind_win_test() ->    
    H1 = rank_test_hand("KH 9S 5H QD QC QH 3S"),
    H2 = rank_test_hand("AC KC KD KS 7H 9D 4D"),
    H3 = rank_test_hand("KS TS QD QS QH 4C 5D"),
    ?assertEqual(?HC_THREE_KIND, H1#hand.rank),
    ?assertEqual(?HC_THREE_KIND, H2#hand.rank),
    ?assertEqual(?HC_THREE_KIND, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H3) > short(H1)).

straight_win_test() ->
    H1 = rank_test_hand("KC QS JH TC 9C 4D 3S"),
    H2 = rank_test_hand("AC KS QH JC TC 9D 4D"),
    H3 = rank_test_hand("KS QD JS TC 9S 2D 7S"),
    ?assertEqual(?HC_STRAIGHT, H1#hand.rank),
    ?assertEqual(?HC_STRAIGHT, H2#hand.rank),
    ?assertEqual(?HC_STRAIGHT, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H1) == short(H3)).

flush_win_test() ->
    H1 = rank_test_hand("4D JD 5D JC QD 2D 7H"),
    H2 = rank_test_hand("8C AD 5D AS KD 9D 4D"),
    H3 = rank_test_hand("4C JC 5C 8D QC 3C 7S"),
    H4 = rank_test_hand("4C JC 7C 8D QC 5C 7S"),
    ?assertEqual(?HC_FLUSH, H1#hand.rank),
    ?assertEqual(?HC_FLUSH, H2#hand.rank),
    ?assertEqual(?HC_FLUSH, H3#hand.rank),
    ?assertEqual(?HC_FLUSH, H4#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H3) > short(H1)),
    ?assertEqual(true, short(H4) > short(H1)).

four_kind_win_test() ->
    H1 = rank_test_hand("4D AS 5H QD QC QH QS"),
    H2 = rank_test_hand("AC KC KD KS KH 9D 4D"),
    H3 = rank_test_hand("KS TS QD QS QH QC 5D"),
    ?assertEqual(?HC_FOUR_KIND, H1#hand.rank),
    ?assertEqual(?HC_FOUR_KIND, H2#hand.rank),
    ?assertEqual(?HC_FOUR_KIND, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H1) > short(H3)).

straight_flush_win_test() ->
    H1 = rank_test_hand("KC QC JC TC 9C 4D AS"),
    H2 = rank_test_hand("AC KC QC JC TC 9D 4D"),
    H3 = rank_test_hand("KS QS JS TS 9S AD 7S"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H1#hand.rank),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H2#hand.rank),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H1) == short(H3)).

full_house_win_test() ->
    H1 = rank_test_hand("4D JS 5H JD JC QH QS"),
    H2 = rank_test_hand("AC AD KD AS KH 9D 4D"),
    H3 = rank_test_hand("3S JH JD JS KH KC 5D"),
    ?assertEqual(?HC_FULL_HOUSE, H1#hand.rank),
    ?assertEqual(?HC_FULL_HOUSE, H2#hand.rank),
    ?assertEqual(?HC_FULL_HOUSE, H3#hand.rank),
    ?assertEqual(true, short(H2) > short(H1)),
    ?assertEqual(true, short(H2) > short(H3)),
    ?assertEqual(true, short(H3) > short(H1)).

two_pair_win1_test() ->
    H1 = rank_test_hand("5C TC 7H KH 5S TS KS"),
    H2 = rank_test_hand("5C TC 7H KH 5S KC TH"),
    ?assertEqual(?HC_TWO_PAIR, H1#hand.rank),
    ?assertEqual(?HC_TWO_PAIR, H2#hand.rank),
    ?assertEqual(true, short(H1) == short(H2)).

high_card_win1_test() ->
    H1 = rank_test_hand("KH TC 9H 7D 6H 5D 2S"),
    H2 = rank_test_hand("KH TC 9H 7H 6H 3D 2S"),
    ?assertEqual(?HC_HIGH_CARD, H1#hand.rank),
    ?assertEqual(?HC_HIGH_CARD, H2#hand.rank),
    ?assertEqual(true, short(H1) == short(H2)).

full_house_win1_test() ->
  H1 = rank_test_hand("2H 2C 5H 5S 5C 7C 4D"),
  H2 = rank_test_hand("2H 2C 5H 5S 5D 4D 2D"),
  ?assertEqual(?HC_FULL_HOUSE, H1#hand.rank),
  ?assertEqual(?HC_FULL_HOUSE, H2#hand.rank),
  ?assertEqual(true, short(H1) == short(H2)).

gr(L) ->
  F = fun({Cards, Rank, High1, High2, Suit}) ->
      R = rank_test_player_hand(Cards),
      ?assertEqual(Rank, R#player_hand.rank),
      ?assertEqual(High1, R#player_hand.high1),
      ?assertEqual(High2, R#player_hand.high2),
      ?assertEqual(Suit, R#player_hand.suit)
  end,
  lists:map(F, L).

rank_player_pair_test() ->
  L = [
    {"2H 2C", ?CF_TWO}, 
    {"5H 5C", ?CF_FIVE}, 
    {"4D 3S 3H", ?CF_THREE}, 
    {"AD 4S 9H 8D 9S 2D TS", ?CF_NINE}
  ],
  gr([{Cards, ?HC_PAIR, High1, ?UNDEF, ?UNDEF} || {Cards, High1} <- L]).

rank_player_two_pair_test() ->
  L = [
    {"2H 2C AD AS", ?CF_ACE, ?CF_TWO},
    {"4H 4C 8D AS AD", ?CF_ACE, ?CF_FOUR},
    {"4H 4C 8D AS AD 8S", ?CF_ACE, ?CF_EIGHT},
    {"KH KC 8D AS AD 8S 7D", ?CF_ACE, ?CF_KING}
  ],
  gr([{Cards, ?HC_TWO_PAIR, High1, High2, ?UNDEF} || {Cards, High1, High2} <- L]).

rank_player_three_kind_test() ->
  L = [
    {"2H 2C 2D AS", ?CF_TWO},
    {"4H 9C AH AS AD", ?CF_ACE},
    {"4H 8C 8D AS 2D 8S", ?CF_EIGHT},
    {"KH KC 6D AS KD 8S 7D", ?CF_KING}
  ],
  gr([{Cards, ?HC_THREE_KIND, High1, ?UNDEF, ?UNDEF} || {Cards, High1} <- L]).

rank_player_four_kind_test() ->
  L = [
    {"2H 2C 2D 2S", ?CF_TWO},
    {"4H AC AH AS AD", ?CF_ACE},
    {"4H 8C 8D 8H 2D 8S", ?CF_EIGHT},
    {"KH KC KS AS KD 8S 7D", ?CF_KING}
  ],
  gr([{Cards, ?HC_FOUR_KIND, High1, ?UNDEF, ?UNDEF} || {Cards, High1} <- L]).

rank_player_full_house_test() ->
  L = [
    {"2H 2C 2D 4S 4D", ?CF_TWO, ?CF_FOUR},
    {"KD AH AC AS 3S 3D", ?CF_ACE, ?CF_THREE},
    {"TS TD KH KC KD 8S 8C", ?CF_KING, ?CF_TEN}
  ],
  gr([{Cards, ?HC_FULL_HOUSE, High1, High2, ?UNDEF} || {Cards, High1, High2} <- L]).

rank_player_staright_test() ->
  L = [
    {"2D 3S 4D 5S 6H", ?CF_SIX},
    {"2D 3S 4D 5S AH", ?CF_FIVE},
    {"2D 3S 4D 5S AH 8D 2S", ?CF_FIVE},
    {"2D 3S 4D 5S 6H 8D 2S", ?CF_SIX},
    {"7D 3S 4D 5S 6H 2D AD", ?CF_SEVEN},
    {"AD KS QD JS TH 9S 8H", ?CF_ACE}
  ],
  gr([{Cards, ?HC_STRAIGHT, High1, ?UNDEF, ?UNDEF} || {Cards, High1} <- L]).

rank_player_flush_test() ->
  L = [
    {"2D 8D 4D 9D AD", ?CF_ACE, ?CS_DIAMONDS},
    {"2D 3D 4D 5D 6S 7D", ?CF_SEVEN, ?CS_DIAMONDS},
    {"3H 9H TH KH 4H QH 5H", ?CF_KING, ?CS_HEARTS},
    {"3H 9H TH KS 4H QH 5H", ?CF_QUEEN, ?CS_HEARTS}
  ],
  gr([{Cards, ?HC_FLUSH, High1, ?UNDEF, Suit} || {Cards, High1, Suit} <- L]).

rank_player_staright_flush_test() ->
  L = [
    {"2D 3D 4D 5D 6D", ?CF_SIX, ?CS_DIAMONDS},
    {"2D 3D 4D 5D AD", ?CF_FIVE, ?CS_DIAMONDS},
    {"KD JD TD QD AD", ?CF_ACE, ?CS_DIAMONDS},
    {"KD JD TD QD AS 9D 8D", ?CF_KING, ?CS_DIAMONDS}
  ],
  gr([{Cards, ?HC_STRAIGHT_FLUSH, High1, ?UNDEF, Suit} || {Cards, High1, Suit} <- L]).

-endif.
