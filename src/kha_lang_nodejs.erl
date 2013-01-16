-module(kha_lang_nodejs).

-export([get_env/1,
         set_runtime_cmd/1]).

get_env(Config) ->
    proplists:get_value(<<"node_js">>, Config, [default]).

set_runtime_cmd(default) ->
    [];
set_runtime_cmd(Ver0) ->
    Ver = iolist_to_binary(Ver0),
    [<<"nvm use ", Ver/binary>>].
