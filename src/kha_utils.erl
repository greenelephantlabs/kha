-module(kha_utils).

-export([to_int/1,
         convert/2,
         fmt/2,
         b2a/1,
         a2b/1,
         i2b/1,
         b2i/1]).

-export([headers/0]).

-export([sh/1,
         sh/2]).

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

headers() ->
    [{<<"Content-Type">>, <<"application/json">>},
     {<<"Cache-Control">>, <<"max-age=0, private">>},
     {<<"Date">>, <<"Sun, 03 Jun 2012 16:31:11 GMT">>},
     {<<"Expires">>, <<"Sun, 03 Jun 2012 16:31:10 GMT">>}].


%% @doc Exec given command.
%% @throws {exec_error, {Command, ErrCode, Output}}.
-spec sh(list(), list()) -> list().
sh(Command, Opts0) ->
    Port = open_port({spawn, Command}, Opts0 ++ [
        exit_status, {line, 255}, stderr_to_stdout
    ]),

    case sh_receive_loop(Port, []) of
        {ok, Data} -> Data;
        {error, {ErrCode, Output}} ->
            throw({exec_error, {Command, ErrCode, Output}})
    end.

sh(Command) ->
    sh(Command, []).

sh_receive_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} -> sh_receive_loop(Port, [Line ++ "\n"|Acc]);
        {Port, {data, {noeol, Line}}} ->
            sh_receive_loop(Port, [Line|Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, E}} ->
            {error, {E, lists:flatten(lists:reverse(Acc))}}
    end.
