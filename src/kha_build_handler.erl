-module(kha_build_handler).

-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         process_post/2,
         delete_resource/2,
         get_json/2,
         put_json/2]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

allowed_methods(Req, State) ->
    {['HEAD', 'GET', 'PUT', 'POST', 'DELETE'], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, put_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

process_post(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    Build = kha_model_build:to_record(Body),
    kha_model_build:save(Build),

    NewId = kha_model_build:get(id, Build),
    {ok, Req2} = cowboy_http_req:set_resp_header(
                   <<"Location">>, <<"/builds/", NewId/binary>>, Req1),

    {true, Req2, State}.

delete_resource(Req, State) ->
    {BuildId, Req1} = cowboy_http_req:binding(build, Req),
    kha_model_build:delete(BuildId),
    {true, Req1, State}.

put_json(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {BuildId, Req2} = cowboy_http_req:binding(build, Req1),

    Build = kha_model_build:to_record(Body),
    Build2 = kha_model_build:set([{id, BuildId}], Build),
    kha_model_build:update(Build2),
    
    {true, Req2, State}.

get_json(Req, State) ->
    JsonModels = lists:foldr(fun(X, <<"">>) ->
                                 X;
                            (X, Acc) ->
                                 <<Acc/binary, ",", X/binary>>
                         end, <<"">>, [kha_model_build:to_json(Model) || Model <- kha_model_build:all()]),

    {<<"[", JsonModels/binary, "]">>, Req, State}.
