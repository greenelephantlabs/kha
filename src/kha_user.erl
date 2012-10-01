-module(kha_user).

-export([fetch/1,
         
         add/2]).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/auth.hrl").

fetch(Email) ->
    db:get_record(user, Email).    

add(Email, Password) ->
    db:add_record(#user{email = Email, password = Password}).
