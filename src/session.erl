-module(session).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/auth.hrl").

-export([login/3,
         logout/1,

         get/1]).

login(Username, Password, Req) ->
    case db:get_record(user, Username) of
        {ok, #user{password = Password}} ->
            SId = hex:to(crypto:rand_bytes(16)),
            Session = #session{id = SId,
                               username = Username,
                               start = now()},
            db:add_record(Session),
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
            case db:get_record(cookie, kha_utils:convert(S, bin)) of
                {ok, Session} ->
                    {ok, Session, Req2};
                {error, _} ->
                    {undefined, Req2}
            end
    end.
