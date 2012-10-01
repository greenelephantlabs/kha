-module(kha_user_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).


init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    Req = session:init(Req0),
    {Method0, Req2} = cowboy_req:method(Req),
    Method = list_to_existing_atom(binary_to_list(Method0)),
    {Url, Req3} = cowboy_req:path(Req2),
    Url2 = binary:split(Url, <<"/">>),
    {ResponseData, Code, Req4} = do(Method, Url2, Req3),
    {ok, Req5} = cowboy_req:reply(Code, kha_utils:headers(),
                                  jsx:to_json(ResponseData), Req4),
    {ok, Req5, State}.

terminate(_Req, _State) ->
    ok.


do('GET', [<<"user">>, <<"session">>], Req) ->
    Session = session:load(),
    Response = session:to_plist(Session),
    {Response, 200, Req}.
