-module(bits_test).
-include("openpoker_test.hrl").

%% 计算有效位（位值为1）的数量
bits0_test() ->
  L = [{1, 2#0001},
       {2, 2#1001},
       {1, 2#0100},
       {0, 2#0000},
       {3, 2#1101}],
  [?assertEqual(Out, bits:bits0(In)) || {Out, In} <- L].

%% 从高位保留有效位
clear_extra_bits_test() ->
  L = [{2#00010110, 2, 2#00010100},
       {2#10011101, 1, 2#10000000},
       {2#01000101, 3, 2#01000101},
       {2#00000101, 5, 2#00000101},
       {2#11010101, 2, 2#11000000}],
  [?assertEqual(Out, bits:clear_extra_bits(In1, In2)) || {In1, In2, Out} <- L].
