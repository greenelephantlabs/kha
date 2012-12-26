-module(kha_config).

-export([fetch/2,

         get_env/1]).

implementations() ->
    [kha_config_github,
     kha_config_archive
     %% kha_config_clone, %%TODO: naive clone depth=1
    ].

get_env(Config) ->
    Env = proplists:get_value("env", Config, []),
    case Env of
        [X|_] when is_tuple(X) ->
            Global0 = proplists:get_value("global", Env, []),
            Global = parse_env(Global0),
            Matrix0 = proplists:get_value("matrix", Env, []),
            Matrix = parse_env(Matrix0),
            [ lists:concat([M, Global]) || M <- Matrix ];
        _ ->
            parse_env(Env)
    end.

parse_env(Env0) -> %%TODO: handle quoted values
    Env = string:tokens(Env0, " "),
    [ {K, V} || [K, V] <- [ [_,_] = string:tokens(X) || X <- Env ] ].

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
