%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% kha_project module
%%% @end
%%% Created : 31 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_project).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

-behaviour(gen_server).

-export([start/1]).

-export([create/1,
         get/1,
         update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([create_fake/0, upgrade/0]).

%% =============================================================================
%% Server API
%% =============================================================================

start(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

%% =============================================================================
%% DB API
%% =============================================================================

create(Project) ->
    {ok, Response} = db:transaction(fun() -> do_create(Project) end),
    Response.

do_create(Project) ->
    ProjectId = db:get_next_id(project),
    R = Project#project{id = ProjectId},
    ok = db:add_record(R),
    {ok, R}.

get(all) ->
    db:get_match_object(#project{_='_'});

get(Id) ->
    {ok, Response} = db:transaction(fun() -> do_get(Id) end),
    Response.

do_get(Id) ->
    db:get_record(project, Id).

update(Project) ->
    validate(Project),
    db:add_record(Project).

%% =============================================================================
%% gen_server code
%% =============================================================================

-define(POLL_TIME, 60000).

-record(state, {id,
                polling = undefined
               }).

init([Id]) ->
    {ok, Self = #project{server = OldServer,
                         params = Params}} = kha_project:get(Id),
    mnesia:subscribe({table, project, detailed}),
    case OldServer of
        undefined -> ok;
        _ ->
            false = erlang:is_process_alive(OldServer)
    end,
    Self2 = Self#project{server = self()},
    update(Self2),
    Timer =
        case proplists:get_value(<<"polling">>, Params, false) of
            true ->
                erlang:start_timer(?POLL_TIME, self(), poll);
            false ->
                undefined
        end,
    {ok, #state{id = Id, polling = Timer}}.

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({mnesia_table_event, {write, project, #project{id = Id} = New, _Old, _ActivityId}}, #state{id = Id} = State) ->
    ?LOG("New project record: ~p~n", [New]),
    {noreply, State};
handle_info({mnesia_table_event, {delete, project, {project, Id}, _Old, _ActivityId}}, #state{id = Id} = State) ->
    {stop, normal, State};
handle_info({mnesia_table_event, {delete, project, #project{id = Id}, _Old, _ActivityId}}, #state{id = Id} = State) ->
    {stop, normal, State};

handle_info({mnesia_table_event, _}, #state{} = State) ->
    {noreply, State};

handle_info({timeout, Timer, poll}, #state{id = Id,
                                           polling = Timer} = State) ->
    ?LOG("starting poll", []),
    {ok, #project{remote = Remote}} = kha_project:get(Id),
    Refs = git:refs(Remote),
    ?LOG("remote branches: ~p~n", [Refs]),
    [ begin
          case kha_build:get_by_revision(Cid) of
              {ok, []} ->
                  {ok, _Build} = kha_build:create_and_add_to_queue(Id, "Polling", Name,
                                                                   Cid, "polling", []),
                  ?LOG("revision ~s added to queue as ~p", [Cid, _Build#build.key]);
              {ok, _L} ->
                  ?LOG("revision ~s was build ~b times", [Cid, length(_L)]),
                  ok
          end
      end || {Name, Type, Cid} <- Refs, Type /= 'HEAD' ],

    {noreply, State#state{polling = erlang:start_timer(?POLL_TIME, self(), poll)}};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% INTERNALS
%% =============================================================================

validate(P = #project{}) ->
    PaB = lists:all(fun({K, _V}) -> is_binary(K) end, P#project.params),
    {params_are_binaries, true} = {params_are_binaries, PaB},
    NaV = lists:all(fun(#notification{type = Type, params = Params}) -> 
                            is_atom(Type) andalso is_params(Params) 
                    end, P#project.notifications),
    {notifications_are_valid, true} = {notifications_are_valid, NaV},
    true.

is_pvalue(N) when is_number(N) ->
    true;
is_pvalue(B) when is_binary(B) ->
    true;
is_pvalue(L) when is_list(L) ->
    lists:all(fun(X) -> is_binary(X) orelse is_number(X) end, L).

is_params(L) when is_list(L) ->
    lists:all(fun({K, V}) -> is_atom(K) andalso is_pvalue(V) end, L).

%% =============================================================================
%% DEBUG
%% =============================================================================

create_fake() ->
    R = [#project{name   = <<"kha test project">>,
                  local  = <<"/tmp/test_build">>,
                  remote = <<"https://github.com/greenelephantlabs/kha.git">>,
                  build  = [<<"rebar get-deps">>, <<"make">>],
                  params = [{<<"build_timeout">>, 60},
                            {<<"polling">>, true}],
                  notifications = []},
         #project{name   = <<"Oortle">>,
                  local  = <<"/tmp/oortle_build">>,
                  remote = <<"git@github.com:LivePress/oortle.git">>,
                  build  = [<<"./oortle/compile-and-run-tests.sh">>],
                  params = [{<<"build_timeout">>, 600}], %% 10 min
                  notifications = []}
        ],
    [ begin
          {ok, Project} = kha_project:create(X),
          PId = Project#project.id,
          ?LOG("Create fake project - ID: ~b", [PId])
      end || X <- R ].
%% [ kha_build:create(PId, X) || X <- example_builds() ].

%% example_builds() ->
%%     [#build{title    = "Test 1",
%%             branch   = "origin/master",
%%             author   = "Paul Peter Flis",
%%             tags     = ["paul", "peter", "test_branch_1"]
%%            },
%%      #build{title    = "Test 2",
%%             branch   = "test_branch_1",
%%             author   = "Gleb Peregud",
%%             tags     = ["gleber", "peregud", "test_branch_1"]
%%            },
%%      #build{title    = "Test 3",
%%             branch   = "test_branch_2",
%%             author   = "Paul Peregud",
%%             tags     = ["paul", "peregud", "test_branch_2"]
%%            }
%%     ].

upgrade() ->
    {ok, Ps} = db:get_all(project),
    [ kha_project:update(binarize(P)) || P <- Ps ].

binarize(#project{params = Params} = P) ->
    Params2 = [ {kha_utils:convert(K, bin), V} || {K, V} <- Params ],
    P#project{params = Params2}.
