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
    ?LOG("Request: ~p~n", [Url]),
    ?LOG("Response: ~p~n", [ResponseData]),
    {ok, Req5} = cowboy_http_req:reply(Code, kha_utils:headers(),
                                       jsx:to_json(ResponseData), Req4),
    {ok, Req5, State}.

%% Get all builds
do('GET', [_PId], Req) ->
    E = example_builds(),
    Response = [ kha_utils:build_to_term(X) || X <- E ],
    {Response, 200, Req};

%% Get build
do('GET', [_PId, BId], Req) ->
    E = example_builds(BId),
    Response = kha_utils:build_to_term(E),
    {Response, 200, Req};

%% Create new build
do('POST', [Id], Req) ->
    {ok, Data, Req2} = cowboy_http_req:body(Req),
    Branch = proplists:get_value(<<"branch">>, Data),
    {ok, BuildId} = builder:new(Id, {branch, Branch}),
    Response = [{<<"project">> , Id},
                {<<"id">>      , BuildId}],
    {Response, 200, Req2}.

terminate(_Req, _State) ->
    ok.

cut_url([<<"project">>, Id, <<"build">>]) ->
    [kha_utils:convert(Id, int)];
cut_url([<<"project">>, PId, <<"build">>, BId]) ->
    [kha_utils:convert(PId, int),
     kha_utils:convert(BId, int)].

%% ONLY FOR DEBUG !!!
example_builds() ->
    [ example_builds(Id) || Id <- lists:seq(1,3) ].
example_builds(Id) ->
    L = [{1, #build{key      = {1,1},
                    id       = 1,
                    project  = 1,
                    title    = "Test 1",
                    branch   = "test_branch_1",
                    revision = "revision",
                    author   = "Paul Peter Flis",
                    start    = now(),
                    stop     = now(),
                    status   = 'failed',
                    exit     = 123,
                    output   = "output, output",
                    tags     = ["paul", "peter", "test_branch_1"]
                   }},
         {2, #build{key      = {2,1},
                    id       = 2,
                    project  = 1,
                    title    = "Test 2",
                    branch   = "test_branch_1",
                    revision = "revision",
                    author   = "Gleb Peregud",
                    start    = now(),
                    stop     = now(),
                    status   = 'success',
                    exit     = 0,
                    output   = "output, output",
                    tags     = ["gleber", "peregud", "test_branch_1"]
                   }},
         {3, #build{key      = {3,1},
                    id       = 3,
                    project  = 1,
                    title    = "Test 3",
                    branch   = "test_branch_2",
                    revision = "revision",
                    author   = "Paul Peregud",
                    start    = now(),
                    stop     = now(),
                    status   = 'pending',
                    exit     = 0,
                    output   = "output, output",
                    tags     = ["paul", "peregud", "test_branch_2"]
                   }}
        ],
    proplists:get_value(Id, L).
