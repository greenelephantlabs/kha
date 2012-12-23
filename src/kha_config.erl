-module(kha_config).

-export([check/1, fetch/2]).

implementations() ->
    [kha_config_github,
     kha_config_archive].

check(Project) ->
    check(Project, implementations()).

check(_Project, [M]) ->
    M;
check(Project, [M | R]) ->
    case M:check(Project) of
        true ->
            M;
        false ->
            check(Project, R)
    end.

fetch(Project, Build) ->
    M = check(Project),
    M:fetch(Project, Build).
