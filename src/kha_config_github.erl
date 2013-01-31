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
    case check(Project) of
        true ->
            fetch0(Project, Build);
        false ->
            {error, incompatible_project}
    end.

fetch0(Project, Build) ->
    Rev = kha_build:get_rev(Build),
    Remote0 = kha_utils:convert(Project#project.remote, str),
    Remote = case re:run(Remote0, "://") of
                 {match, _} -> Remote0;
                 nomatch -> "http://"++re:replace(Remote0, ":", "/", [{return,list}])
             end,
    Uri = uri:from_string(Remote),
    case string:to_lower(uri:host(Uri)) of
        "github.com" ->
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
                    try yamerl:decode(Data, [str_node_as_binary]) of
                        T -> {ok, T}
                    catch
                        ET:ER ->
                            {error, {ET, ER}}
                    end;
                {ok, {{_, 404, _}, _Headers, _Data}} ->
                    {error, nofile};
                {ok, {ErrorCode, _Headers, _Data}} ->
                    {error, {http, ErrorCode}};
                {error, E} ->
                    {error, E}
            end;
        _ ->
            {error, not_github}
    end.
