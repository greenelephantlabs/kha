-module(kha_build_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).

-include("kha.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.



handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    {Url0, Req3} = cowboy_http_req:path(Req2),
    %% {AId, _MId, Special} = kha_utils:parse_url(Url0),
    %% {Req4, Code, Response, HTTPData} =
    %%     try
    %%         do(Method, AId, Special, Req3b)
    %%     catch
    %%         throw:{error, C, Message, ThrownReq} ->
    %%             ?WARNING("Thrown error! Code: ~p; Message: ~p", [C, Message]),
    %%             {ThrownReq, C, [{error, Message}], undefined};
    %%         throw:{error, Message} ->
    %%             ?WARNING("Thrown error! Message: ~p", [Message]),
    %%             {Req3, 500, [{error, Message}], undefined}
    %%     end,
    io:fwrite("URL: ~p~n~n", [Url0]),
    ResponseData = jsx:to_json([<<"a">>, <<"b">>]),
    {ok, Req5} = cowboy_http_req:reply(Code,
                                       [{<<"Content-Type">>, <<"application/json">>}, %%GP: code duplication!
                                        {<<"Cache-Control">>, <<"max-age=0, private">>},
                                        {<<"Date">>, <<"Sun, 03 Jun 2012 16:31:11 GMT">>},
                                        {<<"Expires">>, <<"Sun, 03 Jun 2012 16:31:10 GMT">>}],
                                       ResponseData, Req3),
    {ok, Req5, State}.

terminate(_Req, _State) ->
    ok.

