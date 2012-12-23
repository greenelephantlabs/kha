-module(kha_config_github).

-export([check/1, fetch/2]).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").
-include_lib("uri/include/uri.hrl").

check(P) ->
    case re:run(P#project.remote, "github.com") of
        {match, _} ->
            true;
        _ ->
            false
    end.

fetch(Project, Build) ->
    Rev = kha_build:get_rev(Build),
    Remote = kha_utils:convert(Project#project.remote, str),
    Uri = uri:from_string(Remote),
    ["/", User, Repo0| _] = filename:split(uri:path(Uri)),
    Repo = filename:basename(Repo0, ".git"),
    ConfigUrl = uri:new(uri:scheme(Uri),
                        "", % userinfo
                        "raw.github.com",
                        "", % port
                        filename:join(["/", User,Repo,Rev,".travis.yml"]),
                        "", % query
                        "" % frag
                       ),
    case httpc:request(uri:to_string(ConfigUrl)) of
        {ok, {{_, 200, _}, _Headers, Data}} ->
            io:format("Data: ~p~n", [Data]),
            yamerl:decode(Data);
        {ok, {ErrorCode, _Headers, _Data}} ->
            {error, {http, ErrorCode}};
        {error, E} ->
            {error, E}
    end.
