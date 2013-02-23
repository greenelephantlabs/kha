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
         update/1, set_param/3, set_build/2,

         to_plist/1, from_plist/1,

         create_from_plist/1, update_from_plist/2]).

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
    validate(Project),
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

set_param(Id, Param0, Value) ->
    Param = kha_utils:convert(Param0, bin),
    {ok, #project{params = Params} = P} = ?MODULE:get(Id),
    Params2 = lists:keystore(Param, 1, Params, {Param, Value}),
    update(P#project{params = Params2}).

set_build(Id, Build0) ->
    Build = kha_utils:convert(Build0, bin),
    {ok, #project{} = P} = ?MODULE:get(Id),
    update(P#project{build = Build}).


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
                         params = Params}} = ?MODULE:get(Id),
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
    ?LOG("starting poll for ~p", [Id]),
    {ok, #project{remote = Remote}} = ?MODULE:get(Id),
    Refs = git:refs(Remote),
    %% ?LOG("remote branches: ~p~n", [Refs]),
    [ begin
          case kha_build:get_by_revision(Cid) of
              {ok, []} ->
                  {ok, _Build} = kha_build:create_and_add_to_queue(Id, "Polling", Name,
                                                                   Cid, "polling", []),
                  ?LOG("revision ~s added to queue as ~p", [Cid, _Build#build.key]);
              {ok, _L} ->
                  %% ?LOG("revision ~s was build ~b times", [Cid, length(_L)]),
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
    lists:all(fun({K, V}) -> is_binary(K) andalso is_pvalue(V) end, L).

%% =============================================================================
%% DEBUG
%% =============================================================================

create_fake() ->
    R = [#project{name   = <<"kha test project">>,
                  remote = <<"https://github.com/greenelephantlabs/kha.git">>,
                  build  = [<<"rebar get-deps">>, <<"make">>],
                  params = [{<<"build_timeout">>, 60},
                            {<<"polling">>, true}],
                  notifications = []},
         #project{name   = <<"Erlsemver">>,
                  remote = <<"https://github.com/gleber/erlsemver.git">>,
                  build  = [<<"make all tests">>],
                  params = [{<<"build_timeout">>, 600}], %% 10 min
                  notifications = []},
         #project{name   = <<"jsx">>,
                  remote = <<"https://github.com/talentdeficit/jsx.git">>,
                  build  = [],
                  params = [{<<"build_timeout">>, 600}], %% 10 min
                  notifications = []}
        ],
    [ validate(X) || X <- R ],
    [ begin
          {ok, Project} = ?MODULE:create(X),
          PId = Project#project.id,
          ?LOG("Create fake project - ID: ~b", [PId])
      end || X <- R ].

upgrade() ->
    mnesia:transform_table(project,
                           fun upgrade/1,
                           record_info(fields, project)),
    {ok, Ps} = db:get_all(project),
    [ ?MODULE:update(upgrade(P)) || P <- Ps ].

upgrade({project, Xid, Xserver, Xname, _Xlocal, Xremote, Xbuild, Xparams, Xnotifications}) ->
    #project{id = Xid, server = Xserver, name = Xname, remote = Xremote,
             build = Xbuild, params = Xparams, notifications = Xnotifications};

upgrade(#project{params = Params, notifications = Notifications} = P) ->
    Params2 = binarize(Params),
    Notifications2 = [ upgrade(N) || N <- Notifications ],
    P#project{params = Params2, notifications = Notifications2};

upgrade(#notification{type = mail} = N) ->
    upgrade(N#notification{type = email});
upgrade(#notification{params = Param} = N) ->
    N#notification{params = binarize(Param)}.

binarize(L) ->
    [ {kha_utils:convert(K, bin), V} || {K, V} <- L ].

from_plist(P) when is_list(P) ->
    from_plist0(#project{}, P).

set_private(#project{id = Id}, Private) ->
    Resp = case Private of
               true -> deny;
               false -> allow
           end,
    acl:define(not_logged, {project, Id}, [read, write], Resp).

create_from_plist(L) ->
    P = from_plist(L),
    Private = proplists:get_value(<<"private">>, L, false),
    set_private(P, Private),
    ?MODULE:create(P).

update_from_plist(P, L) ->
    P2 = from_plist0(P, L),
    Private = proplists:get_value(<<"private">>, L, false),
    set_private(P, Private),
    ?MODULE:update(P2),
    P2.

from_plist0(P, []) ->
    P;
from_plist0(P, [{<<"private">>, _}|R]) ->
    from_plist0(P, R);
from_plist0(P, [{<<"id">>, V}|R]) ->
    from_plist0(P#project{id = kha_utils:convert(V, int)}, R);
from_plist0(P, [{<<"name">>, V}|R]) ->
    from_plist0(P#project{name = kha_utils:convert(V, bin)}, R);
from_plist0(P, [{<<"remote">>, V}|R]) ->
    from_plist0(P#project{remote = kha_utils:convert(V, bin)}, R);
from_plist0(P, [{<<"build">>, V}|R]) ->
    from_plist0(P#project{build = kha_utils:list_convert(V, bin)}, R);
from_plist0(P, [{<<"params">>, V}|R]) ->
    from_plist0(P#project{params = kha_utils:list_convert(V, bin)}, R);
from_plist0(P, [{<<"notifications">>, _V}|R]) ->
    from_plist0(P, R).

to_plist(#project{id            = Id,
                  name          = Name,
                  remote        = Remote,
                  build         = Build,
                  params        = Params,
                  notifications = Notification}) ->
    annotate([{<<"id">>, Id},
              {<<"name">>, kha_utils:convert(Name, bin)},
              {<<"remote">>, kha_utils:convert(Remote, bin)},
              {<<"build">>, kha_utils:list_convert(Build, bin)},
              {<<"params">>, kha_utils:binarize(Params)},
              {<<"notifications">>, [ kha_utils:notification_to_plist(N) || N <- Notification ]}
             ]).

annotate(P) when is_list(P) ->
    PId = proplists:get_value(<<"id">>, P),
    Private = acl:read(not_logged, {project, PId}, write) == deny orelse
        acl:read(not_logged, {project, PId}, read) == deny,
    lists:keystore(<<"private">>, 1, P, {<<"private">>, Private}).
