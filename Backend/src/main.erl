-module(main).
-export([start/0]).

start() -> 
   [io:fwrite("~w -> ~w\n", [N, fermat:fermat(N)]) || N <- lists:seq(1, 20)],
   [io:fwrite("~w,7 -> ~w\n", [N, fermat:test(N,7)]) || N <- lists:seq(1, 20)].

