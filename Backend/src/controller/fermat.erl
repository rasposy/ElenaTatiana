-module(fermat).
-export([mpow/3, mpow/4, fermat/1, test/2]).

% test wether a number is a prime.
% Fermat gives an algorithm detecting if a number is a prime with very high accurancy (a complete algorithm would be to expensive to execute).
% here we have a fast implementation of modular exponentiation (=mpow).

% N^K mod M
% if difference =1, K is trivial (=simple).
% N^1 mod _ = N
mpow(N, 1, _) ->
  N;

%% Calculation with: N^K/2 mod M if K is even.
% N^K mod M 
  % rem = remainder of dividing the first number by the second, same as modulo in other languages.
  % in this case K/2.
mpow(N, K, M) ->
  mpow(K rem 2, N, K, M).

% N^K/2 mod M
  % div = perform the division and return the integer component.
  % 0 is here as placeholder because we pass from 3 to 4 parameter.
mpow(0, N, K, M) ->
  X = mpow(N, K div 2, M),
  % N^K/2 * N^K/2
    % if K is even (=paire)
  (X * X) rem M; 

%% Calculation with: N^K-1 mod M if K is odd.
% N^K-1 mod M
  % underscore (_) are normal variables
  % however, they are ignored by the compiler --> they do not generate warnings
mpow(_, N, K, M) ->
  X = mpow(N, K-1, M),
  % N^K-1 * N
    % if K is odd (= impaire)
  (X * N) rem M. 

% if P=1 it's very likely to be a prime.
fermat(1) ->
  ok;

% if random number R, raised by P-1 mod P (<P), equal 1, then it's very likely to be a prime.
fermat(P) ->
  R = rand:uniform(P-1),
  T = mpow(R,P-1,P),
  if
    T == 1 -> % R^(P-1 mod P) = 1
      ok;
    true ->
      no
  end.

test(_, 0) ->
  ok;
test(P, N) ->
  case fermat(P) of
    ok ->
      test(P, N-1);
    no ->
      no
    end.

start() ->
    io:format("fermat initiated~n"),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).
    