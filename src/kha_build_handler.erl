%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% Cowboy HTTP handler for request to builder
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

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
    {Url, Req3} = cowboy_http_req:path(Req2),
    Ids = cut_url(Url),
    {ResponseData, Code, Req4} = do(Method, Ids, Req3),
    {ok, Req5} = cowboy_http_req:reply(Code, kha_utils:headers(),
                                       jsx:to_json(ResponseData), Req4),
    {ok, Req5, State}.

%% Get all builds
do('GET', [PId], Req) ->
    {ok, E} = kha_build:get(PId, all),
    Response = [ kha_utils:build_to_term(X) || X <- E ],
    {Response, 200, Req};

%% Get build
do('GET', [PId, BId], Req) ->
    {ok, E} = kha_build:get(PId, BId),
    Response = kha_utils:build_to_term(E),
    {Response, 200, Req};

%% Rerun build
do('POST', [PId, BId], Req) ->
    {ok, Old} = kha_build:get(PId, BId),
    New = #build{title    = Old#build.title,
                 branch   = Old#build.branch,
                 revision = Old#build.revision,
                 author   = Old#build.author,
                 tags     = Old#build.tags},
    {ok, NewBId} = kha_build:create(PId, New),
    kha_builder:add_to_queue(PId, NewBId),
    R = [<<"create">>, NewBId],
    {R, 200, Req}.

%% Create new build
%% do('POST', [Id], Req) ->
%%     {ok, Data, Req2} = cowboy_http_req:body(Req),
%%     Branch = proplists:get_value(<<"branch">>, Data),
%%     Branch = proplists:get_value(<<"branch">>, Data),
%%     {ok, BuildId} = builder:new(Id, {branch, Branch}),
%%     Response = [{<<"project">> , Id},
%%                 {<<"id">>      , BuildId}],
%%     {Response, 200, Req2}.

terminate(_Req, _State) ->
    ok.

cut_url([<<"project">>, Id, <<"build">>]) ->
    [kha_utils:convert(Id, int)];
cut_url([<<"project">>, PId, <<"build">>, BId]) ->
    [kha_utils:convert(PId, int),
     kha_utils:convert(BId, int)].
