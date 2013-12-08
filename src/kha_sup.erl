
-module(kha_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_project_server/1]).

%% Supervisor callbacks
-export([init/1]).

-export([priv_path/0, priv_path/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, StartupParams), {I, {I, start_link, StartupParams}, permanent, 5000, Type, [I]}).

-include("common.hrl").
-include("kha.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_project_server(Id) ->
    supervisor:start_child(kha_project_sup, [Id]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([project_sup]) ->
    ProjectSpec = {kha_project, {kha_project, start, []},
                   transient, brutal_kill, worker, [kha_project]},
    {ok, {{simple_one_for_one, 5, 1}, [ProjectSpec]}};

init([]) ->
    Optional = optional_specs([xmpp_backend]),
    Builder = ?CHILD(kha_builder, worker),
    ProjectManager = ?CHILD(kha_project_manager, worker),
    PollerSup = {kha_project_sup,
                 {supervisor, start_link, [{local, kha_project_sup},
                                           ?MODULE, [project_sup]]},
                 permanent,
                 infinity,
                 supervisor,
                 []
                },
    HTTP = configure_cowboy(),
    {ok, { {one_for_one, 5, 10}, [Builder, PollerSup, ProjectManager] ++ Optional ++ HTTP} }.

priv_path() ->
    priv_path(".").
priv_path(Sub) ->
    Priv = case code:priv_dir(mas) of
               {error, _} ->
                   filename:join([filename:dirname(code:where_is_file("kha.app")), "..", "priv"]);
               X when is_list(X) ->
                   X
           end,
    filename:join(Priv, Sub).

configure_cowboy() ->
    Port = application:get_env(kha, port, 8093),

    Dispatch = [
                {'_', [
                       %% USER
                       {"/user/[:user_id]", kha_user_handler, []},

                       %% PROJECT
                       {"/project",                kha_project_handler, []},
                       {"/project/[:project_id]",  kha_project_handler, []},

                       %% BUILD
                       {"/project/[:project_id]/build",              kha_build_handler, []},
                       {"/project/[:project_id]/build/[:build_id]",  kha_build_handler, []},

                       %% HOOKS
                       {"/hooks/[:type]/[:project_id]", kha_hooks_handler, []},

                       %% DEFAULT
                       {"/[...]", cowboy_static, {dir, priv_path("www"),
                                                  [{mimetypes, cow_mimetypes, all}]}}
                      ]}
               ],

    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
                    permanent, 5000, supervisor, [ranch_sup]},

    CowboySupSpec = {cowboy_sup, {cowboy_sup, start_link, []},
                     permanent, 5000, supervisor, [cowboy_sup]},

    Listener = ranch:child_spec(kha_http_listener, 10, ranch_tcp, 
                                [{port, Port}], cowboy_protocol, 
                                [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]),
    [RanchSupSpec, CowboySupSpec, Listener].

optional_specs(Servers) ->
    F = fun(Module) -> 
                ParamName = Module, 
                case application:get_env(ParamName) of
                    {ok, Params} ->
                        ?CHILD(Module, worker, Params);
                    undefined ->
                        []
                end
        end,
    lists:flatten(lists:map(F, Servers)).
