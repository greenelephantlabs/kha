-module(kha_user).

-export([fetch/1,

         add/2]).

-include("common.hrl").
-include("auth.hrl").

fetch(Email) ->
    db:get_record(user, Email).

add(Email, Password) ->
    db:add_record(#user{email = kha_utils:convert(Email, bin),
                        password = erlpass:hash(Password)}).
