-module(kha_config_archive).

-export([check/1, fetch/2]).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

check(_P) ->
    true.

fetch(Project, Build) ->
    Remote = Project#project.remote,
    Rev = kha_build:get_rev(Build),
    Dir = kha_utils:mktemp_dir("config."),
    Cfg = filename:join([Dir, ".travis.yml"]),
    Res = case kha_utils:sh("git archive --remote=\"~s\" ~s \".travis.yml\" | tar xvf -C ~s -", [Remote, Rev, Dir], []) of
              {ok, _} ->
                  case filelib:is_file(Cfg) of
                      true ->
                          try yamerl:decode_file(Cfg) of
                              T -> {ok, T}
                          catch
                              ET:ER ->
                                  {error, {ET, ER}}
                          end;
                      false ->
                          {error, nofile}
                  end;
              {error, Error} ->
                  {error, Error}
          end,
    catch file:delete(Cfg),
    catch file:del_dir(Dir),
    Res.
