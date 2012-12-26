-module(kha_lang_nodejs).

-export([set_runtime_cmd/1]).

set_runtime_cmd(default) ->
    [];
set_runtime_cmd(Ver0) ->
    Ver = iolist_to_binary(Ver0),
    [<<"nvm use ", Ver/binary>>].
