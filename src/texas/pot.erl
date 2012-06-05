-module(pot).

-export([new/0, reset/1, new_stage/1, split/3,
         add/4, pots/1, total/1]).

-record(side_pot, {
          members,
          all_in
         }).

-record(pot, {
          active = [],
          inactive = [],
          current = new_side_pot()
         }).

new_side_pot(AllInAmt, Members) ->
    #side_pot{ 
              all_in = AllInAmt, 
              members = Members 
             }.

new_side_pot(Pot = #side_pot{}) ->
    new_side_pot(Pot#side_pot.all_in, Pot#side_pot.members);

new_side_pot(AllInAmt) 
  when is_number(AllInAmt) ->
    new_side_pot(AllInAmt, gb_trees:empty()).

new_side_pot() ->
    new_side_pot(0, gb_trees:empty()).

new() ->
    #pot{}.

reset(Pot) 
  when is_record(Pot, pot) ->
    new().

new_stage(Pot)
  when is_record(Pot, pot) ->
    Inactive = Pot#pot.inactive 
        ++ Pot#pot.active
        ++ [Pot#pot.current],
    Pot#pot { 
      active = [], 
      inactive = Inactive, 
      current = new_side_pot()
     }.

pots(Pot)
  when is_record(Pot, pot) ->
    [{total(P), P#side_pot.members} || P <- side_pots(Pot)].

add(Pot, Player, Amount, IsAllIn)
  when is_record(Pot, pot) ->
    {P, 0} = add_bet(Pot, Player, Amount, IsAllIn),
    P.

%% Ensure that player belongs to the pot

make_member(Pot, Player) ->
    case gb_trees:lookup(Player, Pot#side_pot.members) of
        {value, Bet } ->
            {Pot, Bet};
        _ -> 
            Members = gb_trees:insert(Player, 0, Pot#side_pot.members),
            NewPot = Pot#side_pot { 
                       members = Members 
                      },
            {NewPot, 0}
    end.

%% Add up to all-in amount if pot is split
%% and simply assign the amount if not

add_bet(Pot, Player, Amount) when is_record(Pot, side_pot) ->
    {NewPot, Bet} = make_member(Pot, Player),
    AllIn = NewPot#side_pot.all_in,
    {Unallocated, Members} = 
        if
            AllIn > 0 ->
                %% Pot is split, figure out
                %% the difference between amount bet
                %% so far and the all-in amount
                Delta = AllIn - Bet,
                if 
                    Delta > 0 ->
                        %% Post all-in
                        U = Amount - Delta,
                        M = gb_trees:enter(Player, AllIn, Pot#side_pot.members),
                        {U, M};
                    true ->
                        %% Posted enough already
                        {Amount, Pot#side_pot.members}
                end;
            true ->
                %% Pot is not split, post Amount
                M = update_counter(Player, Amount, Pot#side_pot.members),
                {0, M}
        end,
    NewPot1 = NewPot#side_pot{ 
                members = Members 
               }, 
    {NewPot1, Unallocated};

add_bet(Pot, Player, Amount) when is_record(Pot, pot) ->
    add_bet(Pot, Player, Amount, false).

add_bet(Pot, Player, Amount, IsAllIn) when is_record(Pot, pot) ->
    %% add to prior pots as needed
    {Active, Unallocated} = allocate_bet(Pot#pot.active, Player, Amount),
    Pot1 = Pot#pot {
             active = Active
            },
    if
        IsAllIn ->
            %% split the pot
            Pot2 = split(Pot1, Player, Unallocated),
            Rest = 0;
        true ->
            {Current, Rest} = add_bet(Pot1#pot.current, Player, Unallocated),
            Pot2 = Pot1#pot {
                     current = Current
                    }
    end,
    {Pot2, Rest}.

allocate_bet(SidePots, Player, Amount) when is_list(SidePots) ->
    lists:mapfoldl(fun(Pot, Unallocated) ->
                           add_bet(Pot, Player, Unallocated)
                   end, 
                   Amount, SidePots).

side_pots(Pot) ->
    Temp = lists:append(Pot#pot.active, Pot#pot.inactive),
    Current = Pot#pot.current,
    lists:filter(fun(P) ->
                         gb_trees:size(P#side_pot.members) > 0
                             andalso total(P) > 0
                 end, [Current|Temp]).

total(Pot) when is_record(Pot, side_pot) ->
    F = fun(X, Acc) -> X + Acc end,
    lists:foldl(F, 0, gb_trees:values(Pot#side_pot.members));

total(Pot) when is_record(Pot, pot) ->
    F = fun(X, Acc) -> 
                Acc + total(X)
        end,
    lists:foldl(F, 0, side_pots(Pot)).

%% Split the pot. Last bet for this player plus
%% the current bet becomes the all-in amount.
%% Bets in excess of the all-in amount are moved 
%% to a new side pot.

split(Pot, Player, Amount) when is_record(Pot, pot) ->
    {OldPot, NewPot} = split(Pot#pot.current, Player, Amount),
    Active = lists:append(Pot#pot.active, [OldPot]),
    Pot#pot { 
      current = NewPot, 
      active = Active 
     };

split(SidePot, Player, Amount) ->
    M = update_counter(Player, Amount, SidePot#side_pot.members),
    SidePot1 = SidePot#side_pot { 
                 members = M 
                },
    Members1 = SidePot1#side_pot.members,
    Bet = gb_trees:get(Player, Members1),
    List = gb_trees:to_list(Members1),
    List1 = lists:filter(fun({Key, Value}) ->
                                 (Key /= Player) and (Value > Bet)
                         end, List),
    List2 = lists:map(fun({Key, Value}) ->
                              {Key, Value - Bet}
                      end, List1),
    NewPot = #side_pot {
      all_in = 0,
      members = gb_trees:from_orddict(List2)
     },
    Members2 = lists:map(fun({Key, Value}) -> 
                                 if 
                                     Value > Bet -> {Key, Bet};
                                     true -> {Key, Value}
                                 end
                         end, List),
    OldPot = SidePot1#side_pot { 
               all_in = Bet, 
               members = gb_trees:from_orddict(Members2)
              },
    {OldPot, NewPot}.

update_counter(Key, Amount, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Old} ->
            Old = gb_trees:get(Key, Tree),
            gb_trees:update(Key, Old + Amount, Tree);
        none ->
            gb_trees:insert(Key, Amount, Tree)
    end.

%%%
%%% Test suite
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_member(Pot, Player) 
  when is_record(Pot, side_pot) ->
    gb_trees:is_defined(Player, Pot#side_pot.members).

%% Pot is split, Delta > 0

split_pot_positive_delta_test() ->
    Pot = new_side_pot(100),
    {NewPot, Amount} = add_bet(Pot, 'P', 120),
    ?assertEqual(20, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, total(NewPot)).

%% Pot is split, Delta <= 0

split_pot_negative_delta_test() ->
    Pot = new_side_pot(100),
    {NewPot, Amount} = add_bet(Pot, 'P', 100),
    ?assertEqual(0, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, total(NewPot)).

%% Pot is not split

pot_not_split_test() ->
    Pot = new_side_pot(),
    {NewPot, Amount} = add_bet(Pot, 'P', 100),
    ?assertEqual(0, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, total(NewPot)),
    {NewPot1, Amount1} = add_bet(NewPot, 'P', 100),
    ?assertEqual(0, Amount1),
    ?assertEqual(200, total(NewPot1)).

%% Split pot

pot_split_test() ->
    Pot = new_side_pot(),
    Pot1 = Pot#side_pot{ 
             members = gb_trees:insert('A', 10, Pot#side_pot.members) 
            },
    Pot2 = Pot1#side_pot{ 
             members = gb_trees:insert('B', 30, Pot1#side_pot.members) 
            },
    Pot3 = Pot2#side_pot{ 
             members = gb_trees:insert('C', 40, Pot2#side_pot.members) 
            },
    {NewPot, SidePot} = split(Pot3, 'A', 10),
    ?assertEqual(20, NewPot#side_pot.all_in),
    ?assertEqual(20, gb_trees:get('A', NewPot#side_pot.members)),
    ?assertEqual(20, gb_trees:get('B', NewPot#side_pot.members)),
    ?assertEqual(20, gb_trees:get('C', NewPot#side_pot.members)),
    ?assertEqual(0, SidePot#side_pot.all_in),
    ?assertEqual(10, gb_trees:get('B', SidePot#side_pot.members)),
    ?assertEqual(20, gb_trees:get('C', SidePot#side_pot.members)),
    ?assertEqual(false, is_member(SidePot, 'A')).

%% % ;;; http://www.homepokertourney.com/allin_examples.htm

all_in_example5_test() ->
    Pot = new(),
    { Pot1, Amt1 } = add_bet(Pot, 'A', 100),
    ?assertEqual(0, Amt1),
    { Pot2, Amt2 } = add_bet(Pot1, 'B', 60, true),
    ?assertEqual(0, Amt2),
    ?assertEqual(40, total(Pot2#pot.current)),
    ?assertEqual(true, is_member(Pot2#pot.current, 'A')),
    ?assertEqual(false, is_member(Pot2#pot.current, 'B')),
    ?assertEqual(120, total(hd(Pot2#pot.active))),
    ?assertEqual(true, is_member(hd(Pot2#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot2#pot.active), 'B')).

all_in_example6_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 100),
    { Pot3, 0 } = add_bet(Pot2, 'C', 60, true),
    ?assertEqual(80, total(Pot3#pot.current)),
    ?assertEqual(true, is_member(Pot3#pot.current, 'A')),
    ?assertEqual(true, is_member(Pot3#pot.current, 'B')),
    ?assertEqual(false, is_member(Pot3#pot.current, 'C')),
    ?assertEqual(180, total(hd(Pot3#pot.active))),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'B')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'C')).

all_in_example7_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 100),
    ?assertEqual(80, total(Pot3#pot.current)),
    ?assertEqual(true, is_member(Pot3#pot.current, 'A')),
    ?assertEqual(true, is_member(Pot3#pot.current, 'C')),
    ?assertEqual(false, is_member(Pot3#pot.current, 'B')),
    ?assertEqual(180, total(hd(Pot3#pot.active))),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'B')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'C')).

all_in_example8_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 100),
    { Pot4, 0 } = add_bet(Pot3, 'D', 500),
    { Pot5, 0 } = add_bet(Pot4, 'A', 250, true),
    { Pot6, 0 } = add_bet(Pot5, 'C', 400),
    %% there's a main pot between all 4 players
    Side1 = lists:nth(1, Pot6#pot.active),
    ?assertEqual(240, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),
    ?assertEqual(true, is_member(Side1, 'D')),
    %% there's a side pot between a, c and d
    Side2 = lists:nth(2, Pot6#pot.active),
    ?assertEqual(870, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'C')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'B')),
    %% there's another side pot between c and d
    Side3 = Pot6#pot.current,
    ?assertEqual(300, total(Side3)),
    ?assertEqual(true, is_member(Side3, 'C')),
    ?assertEqual(true, is_member(Side3, 'D')),
    ?assertEqual(false, is_member(Side3, 'A')),
    ?assertEqual(false, is_member(Side3, 'B')).

all_in_example9_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = add_bet(Pot4, 'A', 10),
    { Pot6, 0 } = add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = add_bet(Pot6, 'D', 10),
    %% player-a folds but is still
    %% member of the last side pot
    Side = lists:last(Pot7#pot.active),
    ?assertEqual(28, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = Pot7#pot.current,
    ?assertEqual(59, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'C')).

all_in_example10_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = add_bet(Pot4, 'A', 2, true),
    { Pot6, 0 } = add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = add_bet(Pot6, 'D', 10),
    Side = lists:nth(1, Pot7#pot.active),
    ?assertEqual(28, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot7#pot.active),
    ?assertEqual(15, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'C')),
    Side2 = Pot7#pot.current,
    ?assertEqual(36, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example11_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 5, true),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 8, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 10),
    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(20, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(9, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'A')),
    Side2 = Pot4#pot.current,
    ?assertEqual(4, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example12_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 10),
    Side = lists:last(Pot4#pot.active),
    ?assertEqual(28, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side2 = Pot4#pot.current,
    ?assertEqual(9, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example13_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 20),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    ?assertEqual(30, total(Pot2)).
-endif.
