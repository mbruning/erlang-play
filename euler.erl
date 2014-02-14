-module(euler).
-export([mult/1, even_fib_sum/1]).

% -- https://projecteuler.net/problem=1

check(N, Num) -> length([true|| X <- Num, N rem X =:= 0]) == 1.
add_if_true(N, Num) -> 
    case check(N, Num) of
        true -> N;
        false -> 0
    end.
mult(N) -> mult(N, 0).
mult(0, Acc) -> Acc;
mult(N, Acc) when N > 0 -> mult(N-1, add_if_true(N-1, [3,5]) + Acc).

% -- https://projecteuler.net/problem=2

even_fib_sum(N) -> even_fib_sum(N, 1, 1).
even_fib_sum(N, New, _) when New >= N -> New;
even_fib_sum(N, New, _) when New == 1 -> even_fib_sum(N, 2, New);
even_fib_sum(N, New, Old) -> even_fib_sum(N, New + Old, New).
