%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% Cowboy HTTP handler for request to builder
%%% @end
%%% Created : 02 Aug 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_hooks_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).

-include("kha.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.



handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Url, Req3} = cowboy_req:path(Req2),
    {Type, ProjectId} = cut_url(tl(binary:split(Url, <<"/">>, [global]))),
    {ResponseData, Code, Req4} = do(Method, Type, ProjectId, Req3),
    {ok, Req5} = cowboy_req:reply(Code, kha_utils:headers(),
                                       jsx:to_json(ResponseData), Req4),
    {ok, Req5, State}.

%% Rerun build by copying
do('POST', github, ProjectId, Req) ->
    {QSList, Req3} = cowboy_req:body_qs(Req),
    Data0 = proplists:get_value(<<"payload">>, QSList),
    Data = jsx:to_term(Data0),
    Info = proplists:get_value(<<"head_commit">>, Data),

    Author = proplists:get_value(<<"author">>, Info),
    AuthorName = proplists:get_value(<<"name">>, Author),

    Message = proplists:get_value(<<"message">>, Info),
    CommitId = proplists:get_value(<<"id">>, Info),
    Branch = proplists:get_value(<<"ref">>, Data),

    case string:str(kha_utils:convert(Message, str), "[ci skip]") of
        0 ->
            {ok, Build} = kha_build:create_and_add_to_queue(ProjectId, Message,
                                                            Branch, CommitId,
                                                            AuthorName,
                                                            [AuthorName, <<"github">>]),
            Response = kha_utils:build_to_plist(Build),
            {Response, 200, Req3};
        _ ->
            {[{}], 204, Req3}
    end;

do(_, _, _, Req) ->
    {<<"Not found">>, 404, Req}.

terminate(_Req, _State) ->
    ok.

cut_url([<<"hooks">>, <<"github">>, ProjectId]) ->
    {github, kha_utils:convert(ProjectId, int)}.
