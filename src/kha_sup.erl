
-module(kha_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_project_server/1]).

%% Supervisor callbacks
-export([init/1]).

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
    {ok, { {one_for_one, 5, 10}, [Builder, PollerSup, ProjectManager] ++ Optional} }.

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
