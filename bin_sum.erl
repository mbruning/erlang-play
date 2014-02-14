-module(bin_sum).
-export([calc_rec/1]).

calc_rec(N) -> calc_rec(N, 0).
calc_rec(0, Acc) -> Acc;
calc_rec(N, Acc) when N > 0 -> calc_rec(N bsr 1, Acc + (N band 1)).
