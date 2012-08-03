%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% Cowboy HTTP handler for request to projecter
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_project_handler).
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

%% Get all projects
do('GET', [], Req) ->
    {ok, E} = kha_project:get(all),
    Response = [ kha_utils:project_to_term(X) || X <- E ],
    {Response, 200, Req};

%% Get project
do('GET', [PId], Req) ->
    {ok, E} = kha_project:get(PId),
    Response = kha_utils:project_to_term(E),
    {Response, 200, Req};

%% Get project
do('POST', [PId], Req) ->
    {ok, E} = kha_project:get(PId),
    {ok, Data0, Req2} = cowboy_http_req:body(Req),
    Data = jsx:to_term(Data0),
    E2 = kha_utils:update_project(E, Data),
    Response = kha_utils:project_to_term(E2),
    {Response, 200, Req2}.

terminate(_Req, _State) ->
    ok.

cut_url([<<"project">>]) ->
    [];
cut_url([<<"project">>, PId]) ->
    [kha_utils:convert(PId, int)].
