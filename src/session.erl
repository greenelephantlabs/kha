-module(session).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/auth.hrl").

-export([init/1,

         login/1, login/3,
         logout/1,

         get/1,
         save/1, load/0,

         to_plist/1]).

-compile({no_auto_import,[get/1]}).

init(Req) -> %% stores session record in process dictionary, to be called at the beginning of handlers
    case load() of
        undefined ->
            case get(Req) of
                {ok, _Session, Req2} ->
                    Req2;
                {undefined, Req2} ->
                    Req2
            end;
        #session{} = _Session ->
            Req
    end.

login(Req) ->
    {ok, Data0, Req2} = cowboy_req:body(Req),
    Data = jsx:to_term(Data0),
    login(proplists:get_value(<<"email">>, Data),
          proplists:get_value(<<"password">>, Data),
          Req2).

login(Email, Password, Req) ->
    case kha_user:fetch(Email) of
        {ok, #user{password = Hash}} ->
            case erlpass:match(Password, Hash) of
                true ->
                    SId = hex:to(crypto:rand_bytes(16)),
                    Session = #session{id = SId,
                                       email = Email,
                                       start = now()},
                    db:add_record(Session),
                    save(Session),
                    Req2 = cowboy_req:set_resp_cookie(<<"session">>, SId, [{max_age, 604800}], Req), %% one week
                    {ok, Session, Req2};
                false ->
                    {error, Req}
            end;
        {error, _} ->
            {error, Req}
    end.

logout(Req) ->
    {ok, Session} = session:get(Req),
    db:remove_object(Session),
    Req2 = cowboy_req:set_resp_cookie(<<"session">>, <<"">>, Req),
    {ok, Req2}.

get(Req) ->
    case cowboy_req:cookie(<<"session">>, Req) of
        {undefined, Req2} ->
            {undefined, Req2};
        {S, Req2} ->
            #session{id = S} = load(), %% validate the session
            case db:get_record(cookie, kha_utils:convert(S, bin)) of
                {ok, Session} ->
                    {ok, Session, Req2};
                {error, _} ->
                    {undefined, Req2}
            end
    end.

save(Session) ->
    erlang:put(session, Session).

load() ->
    erlang:get(session).

to_plist(#session{id    = Id,
                  email = Name}) ->
    [{<<"id">>, Id},
     {<<"name">>, kha_utils:convert(Name, bin)}
    ].
