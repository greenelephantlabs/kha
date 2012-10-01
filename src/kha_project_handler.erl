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

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    Req = session:init(Req0),
    {Method0, Req2} = cowboy_req:method(Req),
    Method = list_to_existing_atom(binary_to_list(Method0)),
    {Url, Req3} = cowboy_req:path(Req2),
    Ids = cut_url(Url),
    {ResponseData, Code, Req4} = try
                                     do(Method, Ids, Req3)
                                 catch
                                     throw:{R, C, Rq} ->
                                         {R, C, Rq}
                                 end,
    {ok, Req5} = cowboy_req:reply(Code, kha_utils:headers(),
                                  jsx:to_json(ResponseData), Req4),
    {ok, Req5, State}.

%% Get all projects
do('GET', [], Req) ->
    check(Req, default, read),
    {ok, E} = kha_project:get(all),
    Response = [ kha_utils:project_to_plist(X) || X <- E ],
    {Response, 200, Req};

%% Get project
do('GET', [PId], Req) ->
    check(Req, PId, read),
    {ok, E} = kha_project:get(PId),
    Response = kha_utils:project_to_plist(E),
    {Response, 200, Req};

%% Add new project
do('POST', [], Req) ->
    check(Req, default, write),
    {ok, Data0, Req2} = cowboy_req:body(Req),
    Data = jsx:to_term(Data0),
    E2 = kha_utils:update_project(#project{}, Data),
    {ok, E3} = kha_project:create(E2),
    Response = kha_utils:project_to_plist(E3),
    {Response, 200, Req2};

%% Update project
do('POST', [PId], Req) ->
    check(Req, PId, [read, write]),
    {ok, E} = kha_project:get(PId),
    {ok, Data0, Req2} = cowboy_req:body(Req),
    Data = jsx:to_term(Data0),
    E2 = kha_utils:update_project(E, Data),
    kha_project:update(E2),
    Response = kha_utils:project_to_plist(E2),
    {Response, 200, Req2}.

terminate(_Req, _State) ->
    ok.

cut_url(<<"/project">>) ->
    [];
cut_url(<<"/project/", PId/binary>>) ->
    [kha_utils:convert(PId, int)].

check(Req, default, Operation) ->
    check0(Req, default, Operation);
check(Req, PId, Operation) ->
    check0(Req, {project, PId}, Operation).

check0(Req, PId, Operation) ->
    case acl:check(session:as_acl(), PId, Operation) of
        allow ->
            ok;
        deny ->
            throw({"", 401, Req})
    end.
