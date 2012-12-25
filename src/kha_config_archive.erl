-module(kha_config_archive).

-export([check/1, fetch/2]).

check(_P) ->
    true.

fetch(_Project, _Build) ->
    Dir = kha_utils:mktemp_dir("config."),
    Cfg = filename:join([Dir, ".travis.yml"]),
    Res = case kha_utils:sh("git archive --remote=\"~s\" ~s \".travis.yml\" | tar xvf -C ~s -", [Remote, Rev, Dir], []) of
              {ok, _} ->
                  case filelib:is_file(Cfg) of
                      true ->
                          yamerl:decode_file(Cfg);
                      false ->
                          {error, nofile}
                  end;
              {error, Error} ->
                  {error, Error}
          end,
    catch file:delete(Cfg),
    catch file:del_dir(Dir),
    Res.
    
    
