-module(mobius).
-export([is_prime/1, prime_factors/1, is_square_multiple/1, find_square_multiples/2]).

% Проверка, является ли число простым
is_prime(N) when N > 1 ->
    is_prime(N, 2, round(math:sqrt(N))).

is_prime(_, Divisor, SqrtN) when Divisor > SqrtN ->
    true;
is_prime(N, Divisor, SqrtN) ->
    case N rem Divisor of
        0 -> false;
        _ -> is_prime(N, Divisor + 1, SqrtN)
    end.

% Поиск простых сомножителей числа N
prime_factors(N) when N > 1 ->
    prime_factors(N, 2, []).

prime_factors(1, _, Factors) ->
    Factors;
prime_factors(N, Factor, Factors) ->
    case N rem Factor of
        0 ->
            prime_factors(N div Factor, Factor, [Factor | Factors]);
        _ ->
            prime_factors(N, Factor + 1, Factors)
    end.

% Проверка, делится ли число на квадрат простого числа
is_square_multiple(N) ->
    Factors = prime_factors(N),
    SortedFactors = lists:sort(Factors),
    is_square_multiple(SortedFactors, 0).

is_square_multiple([], _) ->
    false;
is_square_multiple([_], _) ->
    false;
is_square_multiple([X, X | _], _) ->
    true;
is_square_multiple([_ | T], Count) when Count > 0 ->
    is_square_multiple(T, Count - 1);
is_square_multiple([_ | T], 0) ->
    is_square_multiple(T, 1).



find_square_multiples(Count, MaxN) when Count > 0, MaxN > 1 ->
    find_square_multiples(Count, MaxN, 2, 0).

find_square_multiples(0, _, _, _) ->
    fail;
find_square_multiples(Count, MaxN, N, Consecutive) when N =< MaxN ->
    case is_square_multiple(N) of
        true ->
            case Consecutive + 1 of
                Count ->
                    N - Count + 1;
                _ ->
                    find_square_multiples(Count, MaxN, N + 1, Consecutive + 1)
            end;
        false ->
            find_square_multiples(Count, MaxN, N + 1, 0)
    end;
find_square_multiples(_, _, _, _) ->
    fail.
