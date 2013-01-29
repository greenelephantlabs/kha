-module(kha_lang_erlang).

-export([get_env/1,
         set_runtime_cmd/1]).

get_env(Config) ->
    proplists:get_value(<<"otp_release">>, Config, [default]).

set_runtime_cmd(default) ->
    [];
set_runtime_cmd(Ver0) ->
    Ver = iolist_to_binary(string:to_upper(kha_utils:convert(Ver0, str))),
    [<<". `kerl list installations | egrep -i '^", Ver/binary, " ' | awk '{print $2}'`/activate">>].
