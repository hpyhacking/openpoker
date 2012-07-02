-module(pickle).

%%%
%%% Serialization tools
%%%

-export([pickle/2, unpickle/2]).
-export([byte/0, short/0, sshort/0, int/0, 
         sint/0, long/0, slong/0, price/0]).
-export([list/2, choice/2, optional/1, wrap/2,
         tuple/1, record/2, binary/1, wstring/0]).
-export([string/0]).

-include("genesis_common.hrl").

%%% Pickle and unpickle. We accumulate into a list.

pickle({Pickler, _}, Value) ->
    lists:reverse(Pickler([], Value)).

unpickle({_, Pickler}, Bin) ->
    element(1, Pickler(Bin)).

%%% Byte

byte() -> 
    {fun write_byte/2, fun read_byte/1}.

write_byte(Acc, ?UNDEF) ->
    [<<0:8>>|Acc];
write_byte(Acc, Byte) -> 
    [<<Byte:8>>|Acc].

read_byte(Bin) -> 
    <<Byte:8, Rest/binary>> = Bin,
    {Byte, Rest}.

%%% Unsigned short

short() -> 
    {fun write_short/2, fun read_short/1}.

write_short(Acc, Word) -> 
    [<<Word:16>>|Acc].

read_short(Bin) -> 
    <<Word:16, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Signed short

sshort() -> 
    {fun write_sshort/2, fun read_sshort/1}.

write_sshort(Acc, Word) -> 
    [<<Word:16/signed>>|Acc].

read_sshort(Bin) -> 
    <<Word:16/signed, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Unsigned int

int() -> 
    {fun write_int/2, fun read_int/1}.

write_int(Acc, undefined) -> 
    [<<0:32>>|Acc];

write_int(Acc, Word) -> 
    [<<Word:32>>|Acc].

read_int(Bin) -> 
    <<Word:32, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Signed int

sint() -> 
    {fun write_sint/2, fun read_sint/1}.

write_sint(Acc, Word) -> 
    [<<Word:32/signed>>|Acc].

read_sint(Bin) -> 
    <<Word:32/signed, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Unsigned long

long() -> 
    {fun write_long/2, fun read_long/1}.

write_long(Acc, Word) -> 
    [<<Word:64>>|Acc].

read_long(Bin) -> 
    <<Word:64, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Signed long

slong() -> 
    {fun write_slong/2, fun read_slong/1}.

write_slong(Acc, Word) -> 
    [<<Word:64/signed>>|Acc].

read_slong(Bin) -> 
    <<Word:64/signed, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Price with 5 decimal points

price() -> 
    {fun write_price/2, fun read_price/1}.

write_price(Acc, Word) -> 
    Price = trunc(Word * 10000),
    [<<Price:32>>|Acc].

read_price(Bin) -> 
    <<Word:32, Rest/binary>> = Bin,
    Price = Word / 10000,
    {Price, Rest}.

%%% List. We supply a pickler for list length 
%%% as well as a pickler for list elements.

list(Len, Elem) ->
    {fun(Acc, List) -> write_list(Len, Elem, Acc, List) end, 
     fun(Bin) -> read_list(Len, Elem, Bin) end }.

write_list({Len, _}, {Elem, _}, Acc, List) ->
    Acc1 = Len(Acc, length(List)),
    Fun = fun(A, Acc2) -> Elem(Acc2, A) end,
    lists:foldr(Fun, Acc1, List).

read_list({_, Len}, {_, Elem}, Bin) ->
    {N, Bin1} = Len(Bin),
    read_list(N, [], Elem, Bin1).

read_list(0, Acc, _, Bin) -> {Acc, Bin};
read_list(N, Acc, Elem, Bin) ->
    {E, Bin1} = Elem(Bin),
    read_list(N - 1, [E|Acc], Elem, Bin1).

%%% Alternative selection. This could probably use some
%%% deeper thinking. We take a pickler for the tag
%%% as well as a tuple of two functions. The first one
%%% returns the tag value and a pickler based on the supplied
%%% value. The second one selects a pickler based on a tag value.

choice(Tag, Choice) ->
    {fun(Acc, Value) -> write_choice(Tag, Choice, Acc, Value) end,
     fun(Bin) -> read_choice(Tag, Choice, Bin) end }.

write_choice({Tag, _}, {Choice, _}, Acc, Value) 
  when is_function(Tag), 
       is_function(Choice) ->
    {T, {Pickler, _}} = Choice(Value),
    Acc1 = Tag(Acc, T),
    Pickler(Acc1, Value).

read_choice({_, Tag}, {_, Choice}, Bin) 
  when is_function(Tag), 
       is_function(Choice) ->
    {T, Bin1} = Tag(Bin),
    {_, Pickler} = Choice(T),
    Pickler(Bin1).

%%% Optional value. Use 'none' to indicate no value.

optional(Pickler) ->
    {fun(Acc, Value) -> write_optional(Pickler, Acc, Value) end,
     fun(Bin) -> read_optional(Pickler, Bin) end}.

write_optional(_, Acc, none) ->
    [<<0>>|Acc];

write_optional({Pickler, _}, Acc, Value) ->
    Pickler([<<1>>|Acc], Value).

read_optional({_, Pickler}, Bin) ->
    <<Opt:8, Bin1/binary>> = Bin,
    case Opt of 
        0 -> {none, Bin1};
        _ -> Pickler(Bin1)
    end.

%%% Wrapper. Take a pickler and a wrapper tuple of two functions
%%% where the first one is used to convert the value before 
%%% pickling and the second one after unpickling.

wrap(Wrap, Pickler) ->
  {
    fun(Acc, Value) -> 
        write_wrap(Wrap, Pickler, Acc, Value) 
    end,
    fun(Bin) -> 
        read_wrap(Wrap, Pickler, Bin) 
    end
  }.

write_wrap({Wrap, _}, {Pickler, _}, Acc, Value) ->
    Pickler(Acc, Wrap(Value)).

read_wrap({_, Wrap}, {_, Pickler}, Bin) ->
    {Value, Bin1} = Pickler(Bin),
    {Wrap(Value), Bin1}.

%%% Erlang does not support enumerations but I want to have
%%% {cow, sheep, horse} as well as [{cow, 10}, {sheep, 100}]
%%% and be able to marshal these back and forth. Enumerated 
%%% values start from 1 for the tuple case.

%%% Tuple. Uses a tuple of picklers of the same size.

tuple(Picklers) 
  when is_tuple(Picklers) ->
    wrap({fun tuple_to_list/1, 
          fun list_to_tuple/1}, 
         tuple_0(tuple_to_list(Picklers))).

%%% Record. We rely on Erlang records being tuples
%%% and just add the record tag as the first element
%%% when unpickling the record.

record(Tag, Picklers) 
  when is_tuple(Picklers) ->
    wrap({fun(Record) -> record_to_list(Tag, Record) end,
          fun(List) -> list_to_record(Tag, List) end}, 
         tuple_0(tuple_to_list(Picklers))).

write_tuple_0([], Acc, _) ->
    Acc;

write_tuple_0([{Pickler, _}|Rest], Acc, [Value|Tuple]) ->
    write_tuple_0(Rest, Pickler(Acc, Value), Tuple).

read_tuple_0(Picklers, Bin) ->
    read_tuple_0(Picklers, Bin, []).

read_tuple_0([], Bin, Acc) ->
    {lists:reverse(Acc), Bin};

read_tuple_0([{_, Pickler}|Rest], Bin, Acc) ->
    {Value, Bin1} = Pickler(Bin),
    read_tuple_0(Rest, Bin1, [Value|Acc]).

%%% It's convenient to be able to convert the tuple
%%% to a list first as there's no erlang:prepend_element/2.

tuple_0(Picklers) 
  when is_list(Picklers) ->
    {fun(Acc, Value) -> write_tuple_0(Picklers, Acc, Value) end,
     fun(Bin) -> read_tuple_0(Picklers, Bin) end}.

record_to_list(Tag, Record) 
  when is_atom(Tag) ->
    lists:nthtail(1, tuple_to_list(Record)).

list_to_record(Tag, List) 
  when is_atom(Tag), 
       is_list(List) ->
    list_to_tuple([Tag|List]).

%%% Binary

binary(Size) -> 
    {fun (Acc, Bin) -> write_binary(Size, Acc, Bin) end, 
     fun (Bin) -> read_binary(Size, Bin) end}.

write_binary({Size, _}, Acc, undefined) -> 
    Size(Acc, 0);

write_binary({Size, _}, Acc, Bin) -> 
    Acc1 = Size(Acc, size(Bin)),
    [Bin|Acc1].

read_binary({_, Size}, Bin) -> 
    {N, Bin1} = Size(Bin),
    <<Value:N/binary, Bin2/binary>> = Bin1,
    {Value, Bin2}.

%%% Wide string, little-endian style

wstring() ->
    {fun write_wstring/2, 
     fun read_wstring/1}.

write_wstring(Acc, []) ->
    [<<0:16>>|Acc];

write_wstring(Acc, [H|T]) ->
    write_wstring([<<H:16>>|Acc], T).

read_wstring(Bin) ->
    read_wstring(Bin, []).

read_wstring(<<0:16, Bin/binary>>, Acc) ->
    {lists:reverse(Acc), Bin};

read_wstring(<<X:16, Bin/binary>>, Acc) ->
    read_wstring(Bin, [X|Acc]).

string() ->    
    binary(byte()).

%%%
%%% Unit test
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prep_enum_tuple(Enum)
  when is_tuple(Enum) ->
    prep_enum_tuple(Enum, size(Enum), [], []).

prep_enum_tuple(_, 0, Acc1, Acc2) ->
    {Acc1, Acc2};

prep_enum_tuple(Enum, N, Acc1, Acc2) ->
    prep_enum_tuple(Enum, N - 1, 
                    [{element(N, Enum), N}|Acc1],
                    [{N, element(N, Enum)}|Acc2]).

prep_enum_list(Enum) 
  when is_list(Enum) ->
                                                % expect a list of {tag, #value} pairs
    Inv = fun({Key, Val}) -> {Val, Key} end,
    InvEnum = lists:map(Inv, Enum),
    {Enum, InvEnum}.

wrap_enum(Enum) 
  when is_tuple(Enum) ->
    wrap_enum_1(prep_enum_tuple(Enum));

wrap_enum(Enum) 
  when is_list(Enum) ->
    wrap_enum_1(prep_enum_list(Enum)).

wrap_enum_1({List1, List2}) ->
    F = fun(A, B) -> A < B end,
    %% gb_trees needs an ordered list
    Dict1 = lists:sort(F, List1),
    Dict2 = lists:sort(F, List2),
    Tree1 = gb_trees:from_orddict(Dict1),
    Tree2 = gb_trees:from_orddict(Dict2),
    {fun(Key) -> gb_trees:get(Key, Tree1) end,
     fun(Key) -> gb_trees:get(Key, Tree2) end}.       

enum(Enum, Pickler) ->
    wrap(wrap_enum(Enum), Pickler).

-define(pickle(Value, Pickler),
        fun() ->
                ?assertEqual(Value, 
                             unpickle(Pickler,
                                      list_to_binary(pickle(Pickler, 
                                                            Value))))
        end()).

byte_test() ->
    X = 16#ff,
    ?pickle(X, byte()).

short_test() ->
    X = 16#ffff,
    ?pickle(X, short()).

sshort_test() ->
    X = -1,
    ?pickle(X, sshort()).

int_test() ->
    X = 16#ffffffff,
    ?pickle(X, int()).

sint_test() ->
    X = -1,
    ?pickle(X, sint()).

long_test() ->
    X = 16#aabbccddeeff0011,
    ?pickle(X, long()).

slong_test() ->
    X = -1,
    ?pickle(X, slong()).

list_test() ->
    X = "Wazzup!",
    ?pickle(X, list(int(), byte())).

%%% A choice of serializing either a list or a long.

value2tag(Action) 
  when is_list(Action) ->
    {0, list(byte(), byte())};

value2tag(_) ->
    {1, long()}.

tag2value(0) ->
    list(byte(), byte());

tag2value(1) ->
    long().

selector() ->
    {fun value2tag/1, fun tag2value/1}.

selector_test() ->
    X1 = "Just testing",
    X2 = 16#ffff,
    ?pickle(X1, choice(byte(), selector())),
    ?pickle(X2, choice(byte(), selector())).

%%% Optional value

optional_test() ->
    X1 = none,
    X2 = 55,
    ?pickle(X1, optional(byte())),
    ?pickle(X2, optional(byte())).

%%% Tuple given as a tuple and a list of key/value pairs.

tuple_enum_test() ->
    %% tuple enum
    Enum = {cow, sheep, horse},
    {FROM, TO} = wrap_enum(Enum),
    ?assertEqual(1, FROM(cow)),
    ?assertEqual(2, FROM(sheep)),
    ?assertEqual(3, FROM(horse)),
    ?assertEqual(cow, TO(1)),
    ?assertEqual(sheep, TO(2)),
    ?assertEqual(horse, TO(3)).

list_enum_test() ->
    %% list enum
    Enum = [{cow, 20}, {sheep, 30}, {horse, 40}],
    {FROM, TO} = wrap_enum(Enum),
    ?assertEqual(20, FROM(cow)),
    ?assertEqual(30, FROM(sheep)),
    ?assertEqual(40, FROM(horse)),
    ?assertEqual(cow, TO(20)),
    ?assertEqual(sheep, TO(30)),
    ?assertEqual(horse, TO(40)).

enum_test() ->
    Enum1 = {cow, sheep, horse},
    Enum2 = [{cow, 20}, {sheep, 30}, {horse, 40}],
    ?pickle(cow, enum(Enum1, byte())),
    ?pickle(sheep, enum(Enum2, byte())).

tuple_test() ->
    Tuple = {"Joel", 16#ff00, none},
    Spec = {list(byte(),byte()), short(), optional(byte())},
    ?pickle(Tuple, tuple(Spec)).

%%% Nested records.

-record(foo, { a, b }).
-record(bar, { c, d }).
-record(baz, { e, f }).

nested_record_test() ->
    R1 = #foo { 
      a = 10, 
      b = #bar {
        c = 20, 
        d = #baz {
          e = 30,
          f = "Enough nesting!"
         }
       }
     },
    Pickler = record(foo, {
                       byte(),
                       record(bar, {
                                int(),
                                record(baz, {
                                         sshort(),
                                         list(byte(), byte())
                                        })
                               })
                      }),
    ?pickle(R1, Pickler).

%%% Binary

binary_test() ->
    X = <<1, 2, 3, 4>>,
    ?pickle(X, binary(int())).

%%% Wide string

wstring_test() ->
    X = "This is a wide string",
    ?pickle(X, wstring()).
-endif.
