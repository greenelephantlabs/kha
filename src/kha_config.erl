-module(kha_config).

-export([fetch/2,

         get_env/1]).

implementations() ->
    [kha_config_github,
     kha_config_archive
     %% kha_config_clone, %%TODO: naive clone depth=1
    ].

get_env(Config) ->
    case get_env0(Config) of
        [] ->
            [[]];
        R ->
            R
    end.

get_env0(Config) ->
    case proplists:get_value(<<"env">>, Config, []) of
        [X|_] = Env when is_tuple(X) ->
            Global0 = proplists:get_value(<<"global">>, Env, []),
            Global = lists:flatten(lists:map(fun parse_env/1, Global0)),
            Matrix0 = proplists:get_value(<<"matrix">>, Env, []),
            Matrix = lists:map(fun parse_env/1, Matrix0),
            [ lists:append(M, Global) || M <- Matrix ];
        L when is_list(L) ->
            [ parse_env(B) || B <- L ];
        B when is_binary(B) ->
            [ parse_env(B) ]
    end.

parse_env(Env0) -> %%TODO: handle quoted values
    Env = string:tokens(binary_to_list(Env0), " "),
    [ {K, V} || [K, V] <- [ [_,_] = string:tokens(X, "=") || X <- Env ] ].

fetch(Project, Build) ->
    fetch(Project, Build, implementations()).

fetch(_Project, _Build, []) ->
    {error, unable_to_fetch_config};

fetch(Project, Build, [M | R]) ->
    case M:fetch(Project, Build) of
        {ok, Data} ->
            {ok, Data};
        {error, nofile} ->
            {error, nofile};
        {error, _Error} ->
            fetch(Project, Build, R)
    end.
