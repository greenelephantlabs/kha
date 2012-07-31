%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% kha_project module
%%% @end
%%% Created : 31 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_project).

-include("kha.hrl").

-export([create/1,
         get/1]).

-export([create_fake/0]).

create(Project) ->
    {ok, Response} = db:transaction(fun() -> do_create(Project) end),
    Response.

do_create(Project) ->
    ProjectId = db:get_next_id(project),
    R = Project#project{id = ProjectId},
    ok = db:add_record(R),
    {ok, ProjectId}.

get(all) ->
    db:get_match_object(#project{_='_'});

get(Id) ->
    {ok, Response} = db:transaction(fun() -> do_get(Id) end),
    Response.

do_get(Id) ->
    db:get_record(project, Id).


%% ONLY FOR DEBUG !!!
create_fake() ->
    R = #project{name  = <<"kha test project">>,
                 local  = <<"/tmp/test_build">>,
                 remote = <<"https://github.com/greenelephantlabs/kha.git">>,
                 build  = [<<"rebar get-deps">>, <<"make">>],
                 notifications = []},
    {ok, PId} = kha_project:create(R),
    ?LOG("Create fake project - ID: ~b", [PId]),
    [ kha_build:create(PId, X) || X <- example_builds() ].

example_builds() ->
    [#build{title    = "Test 1",
            branch   = "origin/master",
            revision = "revision",
            author   = "Paul Peter Flis",
            stop     = now(),
            tags     = ["paul", "peter", "test_branch_1"]
           },
     #build{title    = "Test 2",
            branch   = "test_branch_1",
            revision = "revision",
            author   = "Gleb Peregud",
            stop     = now(),
            tags     = ["gleber", "peregud", "test_branch_1"]
           },
     #build{title    = "Test 3",
            branch   = "test_branch_2",
            revision = "revision",
            author   = "Paul Peregud",
            stop     = now(),
            tags     = ["paul", "peregud", "test_branch_2"]
           }
    ].
