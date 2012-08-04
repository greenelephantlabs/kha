%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% kha_build module
%%% @end
%%% Created : 31 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_build).

-include("kha.hrl").

-export([create/2,
         create_and_add_to_queue/6,

         get/2,

         delete/1,
         delete/2,

         update/1]).

create(ProjectId, Build) ->
    {ok, Response} = db:transaction(fun() -> do_create(ProjectId, Build) end),
    Response.

do_create(ProjectId, Build) ->
    BuildId = db:get_next_id({build, ProjectId}),
    R = Build#build{key = {ProjectId, BuildId},
                    id = BuildId,
                    project = ProjectId,
                    start = now(),
                    status = 'pending',
                    exit = -1, %% FIXME: PF: Should by 'undefined'
                    output = []},
    ok = db:add_record(R),
    {ok, R}.

create_and_add_to_queue(ProjectId, Title, Branch, Revision, Author, Tags) ->
    New = #build{title    = Title,
                 branch   = Branch,
                 revision = Revision,
                 author   = Author,
                 tags     = Tags},
    {ok, Build} = kha_build:create(ProjectId, New),
    kha_builder:add_to_queue(Build),
    {ok, Build}.

get(ProjectId, all) ->
    db:get_match_object(#build{key={ProjectId, '_'}, _='_'});

get(ProjectId, BuildId) ->
    {ok, Response} = db:transaction(fun() -> do_get(ProjectId, BuildId) end),
    Response.

do_get(ProjectId, BuildId) ->
    db:get_record(build, {ProjectId, BuildId}).

delete(#build{} = Build) ->
    db:remove_object(Build).

delete(ProjectId, BuildId) ->
    db:remove_record(build, {ProjectId, BuildId}).


update(Build) ->
    db:add_record(Build).
