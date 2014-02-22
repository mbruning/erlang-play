-module(euler).
-export([mult/1,
         even_fib_sum/1,
         prime_factor/1,
         palind_prod/1,
         palind_prod/2]).
-include_lib("/usr/local/lib/erlang/lib/eunit-2.2.6/include/eunit.hrl").

%% -- util functions

check_rem(N, Num) -> length([true|| X <- Num, N rem X =:= 0]) >= 1.

add_if_true(N, Num) ->
  case check_rem(N, Num) of
    true -> N;
    false -> 0
  end.

%% -- https://projecteuler.net/problem=1

mult(N) -> mult(N, 0).
mult(0, Acc) -> Acc;
mult(N, Acc) when N > 0 -> mult(N-1, add_if_true(N-1, [3,5]) + Acc).


%% -- https://projecteuler.net/problem=2

even_fib_sum(N) -> even_fib_sum(N, 1, 1, 0).
even_fib_sum(N, New, _, Sum) when New >= N -> Sum;
even_fib_sum(N, New, _, Sum) when New == 1 -> even_fib_sum(N, 2, New, Sum + 2);
even_fib_sum(N, New, Old, Sum) -> even_fib_sum(N, New + Old, New, Sum + add_if_true(New + Old, [2])).

%% -- https://projecteuler.net/problem=3

get_primes([]) -> [];
get_primes([H | T]) -> [H | get_primes(lists:filter(fun (N) -> N rem H =/= 0 end, T))];
get_primes(N) -> get_primes(lists:seq(2,N)).

prime_factor(N) -> lists:filter(fun (X) -> N rem X =:= 0 end, get_primes(N)).

%% -- https://projecteuler.net/problem=4

is_palindrom(Prod) -> lists:reverse(integer_to_list(Prod)) =:= integer_to_list(Prod).

palind_prod(N) -> palind_prod(N, 0.9).
palind_prod(N, Fac) ->
  Max = trunc(math:pow(10, N) - 1),
  palind_prod(Max, Max, Max, 0, Fac).
palind_prod(Max1, Max2, Org, Res, Fac) when Max1 >= Fac * Org, Max2 >= Fac * Org ->
  case Max1 * Max2 > Res of
    true -> case is_palindrom(Max1 * Max2) of
              true -> palind_prod(Max1-1, Max2, Org, Max1 * Max2, Fac);
              false -> palind_prod(Max1-1, Max2, Org, Res, Fac)
            end;
    false -> palind_prod(Max1-1, Max2, Org, Res, Fac)
  end;
palind_prod(Max1, Max2, Org, Res, Fac) when Max1 < Fac * Org, Max2 >= Fac * Org ->
  palind_prod(Max2, Max2-1, Org, Res, Fac);
palind_prod(Max1, Max2, Org, Res, Fac) when Max1 < Fac * Org; Max2 < Fac * Org -> Res.


%% -- unit tests

palind_prod_test_() ->
  [?_assert(palind_prod(2) =:= 9009),
   ?_assert(palind_prod(3) =:= 906609)].

get_primes_test_() ->
  ?_assert(get_primes(10) =:= [2,3,5,7]).

prime_factor_test_() ->
  ?_assert(prime_factor(13195) =:= [5,7,13,29]).

check_test_() ->
  [?_assert(check_rem(4, [4,2])),
   ?_assert(check_rem(6, [3,2])),
   ?_assertNot(check_rem(6, [5]))].

add_if_true_test_() ->
  ?_assertEqual(4, add_if_true(4, [2])).

mult_test_() ->
  ?_assertEqual(23, mult(10)).

even_fib_sum_test_() ->
  ?_assertEqual(44, even_fib_sum(80)).

