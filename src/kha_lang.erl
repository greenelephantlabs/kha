-module(kha_lang).

-export([get_envs/1,
         get_env/1, get_env/2,
         set_runtime_cmd/2]).

languages() ->
    [erlang, nodejs].

get_envs(Config) ->
    lists:map(fun tuple_to_list/1,
              sofs:to_external(
                sofs:product(
                  list_to_tuple(
                    [ sofs:set(
                        [ {K, X} || X <- V ] )
                      || {K, V} <- get_env(Config)
                    ])))).

get_env(Config) ->
    [ {X, get_env(X, Config)} || X <- [ env | languages() ] ].

get_env(X, Config) ->
    (mod(X)):get_env(Config).

mod(env) ->
    kha_config;
mod(Atom) ->
    list_to_atom("kha_lang_" ++ atom_to_list(Atom)).

set_runtime_cmd(Lang, Ver) ->
    (mod(Lang)):set_runtime_cmd(Ver).
