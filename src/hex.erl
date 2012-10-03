-module(hex).
-export([to/1, list_to_hex/1]).

to(S) when is_binary(S) ->
    List = binary_to_list(S),
    lists:flatten(list_to_hex(List));
to(S) ->
    to(term_to_binary(S)).

list_to_hex(L)->
    lists:map(fun int_to_hex/1, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).
