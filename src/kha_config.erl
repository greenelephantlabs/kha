-module(kha_config).

-export([fetch/2]).

implementations() ->
    [kha_config_github,
     kha_config_archive
     %% kha_config_clone, %%TODO: naive clone depth=1
    ].

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
