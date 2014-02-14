-module(euler).
-export([mult/1]).

% -- https://projecteuler.net/problem=1

check(N) when N rem 5 == 0 -> N;
check(N) when N rem 3 == 0 -> N;
check(_) -> 0.
mult(N) -> mult(N, 0).
mult(0, Acc) -> Acc;
mult(N, Acc) when N > 0 -> mult(N-1, check(N-1) + Acc).


