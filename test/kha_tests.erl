-module(kha_tests).

-include_lib("eunit/include/eunit.hrl").

priv_path_test() ->
    Path = kha_sup:priv_path("www"),
    true = lists:suffix("kha/ebin/../priv/www", Path).

kha_test() ->
    %% inets:start(),
    application:set_env(kha, port, 8094),
    kha_app:start(),
    Res = httpc:request("http://localhost:8094/"),
    ?assertMatch({ok, _}, Res),
    kha_app:stop().
