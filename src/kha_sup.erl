
-module(kha_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([project_sup]) ->
    ProjectSpec = {kha_project, {kha_project, start, []},
                   permanent, brutal_kill, worker, [kha_project]},
    {ok, {{simple_one_for_one, 5, 1}, [ProjectSpec]}};

init([]) ->
    Builder = ?CHILD(kha_builder, worker),
    PollerSup = {kha_project_sup,
                 {supervisor, start_link, [{local, kha_project_sup},
                                           ?MODULE, [project_sup]]},
                 permanent,
                 infinity,
                 supervisor,
                 []
                },
    {ok, { {one_for_one, 5, 10}, [Builder, PollerSup]} }.
