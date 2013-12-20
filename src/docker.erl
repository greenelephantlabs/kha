-module(docker).

-export([runner_str/2]).

runner_str(Name, _Opts) ->
    Str = kha_utils:fmts("docker run -i -rm \"~s\"", [Name]),
    {ok, Str}.
