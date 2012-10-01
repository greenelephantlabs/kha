-module(session).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/auth.hrl").

-export([init/1,

         login/3,
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
                {error, Req2} ->
                    Req2
            end;
        #session{} = _Session ->
            Req
    end.

login(Username, Password, Req) ->
    case db:get_record(user, Username) of
        {ok, #user{password = Password}} ->
            SId = hex:to(crypto:rand_bytes(16)),
            Session = #session{id = SId,
                               username = Username,
                               start = now()},
            db:add_record(Session),
            save(Session),
            Req2 = Req:set_resp_cookie(<<"session">>, SId, [{max_age, 604800}]), %% one week
            {ok, Session, Req2};
        {error, _} ->
            {error, Req}
    end.

logout(Req) ->
    {ok, Session} = session:get(Req),
    db:remove_object(Session),
    Req2 = Req:set_resp_cookie(<<"session">>, <<"">>),
    {ok, Req2}.

get(Req) ->
    case Req:cookie(<<"session">>) of
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

to_plist(#session{id       = Id,
                  username = Name}) ->
    [{<<"id">>, Id},
     {<<"name">>, kha_utils:convert(Name, bin)}
    ].
