-module(kha_utils).

-export([to_int/1,
         convert/2,
         fmt/2,
         b2a/1,
         a2b/1,
         i2b/1,
         b2i/1]).


to_int(X) when is_binary(X) -> to_int(binary_to_list(X));
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_integer(X) -> X;
to_int(X) when X==undefined -> undefined;
to_int(X) when is_atom(X) -> to_int(atom_to_list(X)).

fmt(S, A) ->
    convert(io_lib:format(S, A), bin).

b2a(B) when is_binary(B) ->
    binary_to_atom(B, latin1).

a2b(A) when is_atom(A) ->
    atom_to_binary(A, latin1).

i2b(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I)).
b2i(I) when is_binary(I) ->
    list_to_integer(binary_to_list(I)).

convert(Val, int)
  when is_list(Val) ->
    list_to_integer(Val);

convert(Val, int)
  when is_binary(Val) ->
    list_to_integer(binary_to_list(Val));

convert(Val, atom)
  when is_list(Val) ->
    list_to_atom(Val);

convert(Val, atom)
  when is_binary(Val) ->
    b2a(Val);

convert(Val, str)
  when is_integer(Val) ->
    integer_to_list(Val);

convert(Val, str)
  when is_float(Val) ->
    float_to_list(Val);

convert(Val, str)
  when is_atom(Val) ->
    atom_to_list(Val);

convert(Val, str)
  when is_binary(Val) ->
    binary_to_list(Val);

convert(Val, bin)
  when is_list(Val) ->
    iolist_to_binary(Val);

convert(Val, bin)
  when is_atom(Val) ->
    a2b(Val);

convert(Val, bin)
  when is_binary(Val) ->
    Val;

convert(undefined, bool) ->
    false;

convert(false, bool) ->
    false;

convert("", bool) ->
    false;

convert("0", bool) ->
    false;

convert("false", bool) ->
    false;

convert(<<"">>, bool) ->
    false;

convert(<<"0">>, bool) ->
    false;

convert(<<"false">>, bool) ->
    false;

convert(_, bool) ->
    true;

convert(Val, _) ->
    Val.
