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
do('DELETE', [PId, BId], Req) ->
    {ok, E} = kha_build:get(PId, BId),
    kha_build:delete(E),
    Response = [],%%kha_utils:build_to_term(E),
    {Response, 200, Req};

%% Get build
do('GET', [PId, BId], Req) ->
    {ok, E} = kha_build:get(PId, BId),
    Response = kha_utils:build_to_term(E),
    {Response, 200, Req};

%% Rerun build by copying
do('POST', [PId], Req) ->
    {ok, Data0, Req2} = cowboy_http_req:body(Req),
    Data = jsx:to_term(Data0),
    case proplists:get_value(<<"copy">>, Data) of
        undefined ->
            {R, C} = create_build(PId, Data),
            {R, C, Req2};
        BId ->
            {R, C} = copy_build(PId, BId, Data),
            {R,C, Req2}
    end.

%% Rerun existing build
%% do('POST', [PId, BId], Req) ->
%%     {ok, Old0} = kha_build:get(PId, BId),
%%     Old = Old0#build{start = now()},
%%     kha_build:update(Old),
%%     kha_builder:add_to_queue(PId, BId),
%%     R = kha_utils:build_to_term(Old),
%%     {R, 200, Req}.

%% Create new build
create_build(ProjectId, Data) ->
    Title    = proplists:get_value(<<"title">>, Data),
    Branch   = proplists:get_value(<<"branch">>, Data),
    Revision = proplists:get_value(<<"revision">>, Data),
    Author   = proplists:get_value(<<"author">>, Data),
    Tags     = proplists:get_value(<<"tags">>, kha_utils:list_convert(Data, bin)),
    {ok, Build} = kha_build:create_and_add_to_queue(ProjectId, Title, Branch,
                                                    Revision, Author, Tags),
    Response = kha_utils:build_to_term(Build),
    {Response, 200}.

copy_build(ProjectId, BuildId, _Data) ->
    {ok, Old} = kha_build:get(ProjectId, BuildId),
    {ok, Build} = kha_build:create_and_add_to_queue(ProjectId,
                                                    Old#build.title,
                                                    Old#build.branch,
                                                    Old#build.revision,
                                                    Old#build.author,
                                                    Old#build.tags),
    Response = kha_utils:build_to_term(Build),
    {Response, 200}.

    
terminate(_Req, _State) ->
    ok.

cut_url([<<"project">>, Id, <<"build">>]) ->
    [kha_utils:convert(Id, int)];
cut_url([<<"project">>, PId, <<"build">>, BId]) ->
    [kha_utils:convert(PId, int),
     kha_utils:convert(BId, int)].
