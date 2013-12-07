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
         terminate/3]).

-include("common.hrl").
-include("kha.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    Req = session:init(Req0),
    {Method0, Req2} = cowboy_req:method(Req),
    Method = list_to_existing_atom(binary_to_list(Method0)),
    {Url, Req3} = cowboy_req:path(Req2),
    Ids = cut_url(Url),
    {ResponseData, Code, Req4} = acl:web(fun() -> 
                                                 do(Method, Ids, Req3)
                                         end),
    {ok, Req5} = cowboy_req:reply(Code, kha_utils:headers(),
                                  jsx:to_json(ResponseData), Req4),
    {ok, Req5, State}.

%% Get all projects
do('GET', [], Req) ->
    check(Req, default, read),
    {ok, E0} = kha_project:get(all),
    E = [ X || #project{id = PId} = X <- E0, acl:check(session:as_acl(), {project, PId}, read) == allow ],
    Response = [ kha_project:to_plist(X) || X <- E ],
    {Response, 200, Req};

%% Get project
do('GET', [PId], Req) ->
    check(Req, {project, PId}, read),
    {ok, E} = kha_project:get(PId),
    Response = kha_project:to_plist(E),
    {Response, 200, Req};

%% Add new project
do('POST', [], Req) ->
    check(Req, default, write),
    {ok, Data0, Req2} = cowboy_req:body(Req),
    Data = jsx:to_term(Data0),
    {ok, E2} = kha_project:create_from_plist(Data),
    Response = kha_project:to_plist(E2),
    {Response, 200, Req2};

%% Update project
do('POST', [PId], Req) ->
    check(Req, {project, PId}, [read, write]),
    {ok, P} = kha_project:get(PId),
    {ok, Data0, Req2} = cowboy_req:body(Req),
    Data = jsx:to_term(Data0),
    P2 = kha_project:update_from_plist(P, Data),
    Response = kha_project:to_plist(P2),
    {Response, 200, Req2}.

terminate(_,_,_) ->
    ok.

cut_url(<<"/project">>) ->
    [];
cut_url(<<"/project/", PId/binary>>) ->
    [kha_utils:convert(PId, int)].

check(Req, PId, Operation) ->
    acl:web_check(Req, PId, Operation).
