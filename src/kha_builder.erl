%%%-------------------------------------------------------------------
%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% kha_builder module
%%% @end
%%% Created : 31 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>
%%%-------------------------------------------------------------------
-module(kha_builder).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([add_to_queue/1,
         add_to_queue/2,
         process/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([build_timeout/3]).

-define(SERVER, ?MODULE).

-record(state, {busy = false :: 'false' | {pid(), term()},
                queue}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_to_queue(#build{project = Project, id = Id} = _Build) ->
    add_to_queue(Project, Id).
add_to_queue(ProjectId, BuildId) ->
    gen_server:call(?SERVER, {add_to_queue, ProjectId, BuildId}).

process() ->
    gen_server:cast(?SERVER, process).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, PendingBuilds} = db:get_match_object(#build{status=pending,_='_'}),
    Keys = [ B#build.key || B <- PendingBuilds ],
    process(),
    {ok, #state{queue = queue:from_list(Keys)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({add_to_queue, ProjectId, BuildId}, _From,
            #state{busy = Busy, queue = Queue} = S) ->
    NewQueue = queue:in({ProjectId, BuildId}, Queue),
    NewState = S#state{queue = NewQueue},
    case Busy of
        false -> kha_builder:process();
        _ -> do_nothing
    end,
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(process, #state{queue = Queue} = S) ->
    case queue:out(Queue) of
        {empty, _} ->
            {noreply, S#state{busy = false}};
        {{value, Job}, NewQueue} ->
            ContData = fetch_container_data(),
            Pid = proc_lib:spawn(fun() ->
                                         do_process(Job, ContData)
                                 end),
            erlang:monitor(process, Pid),
            {noreply, S#state{busy = {Pid, Job}, queue = NewQueue}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _, process, Pid, normal}, #state{busy = {Pid, _}} = State) ->
    kha_builder:process(),
    {noreply, State#state{busy = false}};

handle_info({'DOWN', _, process, Pid, Reason}, #state{busy = {Pid, Job}} = State) ->
    {ProjectId, BuildId} = Job,
    {ok, Build0} = kha_build:get(ProjectId, BuildId),
    Build2 = case Reason of
                 timeout ->
                     Build0#build{status = timeout};
                 _ ->
                     Build0#build{status = fail}
             end,
    _Build3 = build_append(io_lib:format("# reason: ~w~n", [Reason]), Build2),
    kha_builder:process(),
    {noreply, State#state{busy = false}};

handle_info(_Info, State) ->
    {stop, {unknown_info, _Info}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_timeout(Pid, _ProjectId, _BuildId) ->
    exit(Pid, timeout).

fetch_container_data() ->
    case application:get_env(container) of
        undefined ->
            {dummy, "dummy", []};
        {ok, ContType} ->
            {ok, Name} = application:get_env(container_name),
            Opts = case application:get_env(container_opts) of
                       {ok, O} -> O;
                       undefined -> []
                   end,
            {ContType, Name, Opts}
    end.

container_start({Module, Name, Opts}) ->
    {ok, CPid} = kha_cont:start_link(Module, Name, Opts),
    {ok, CPid}.

container_wait(CPid, Build) ->
    {ok, Name} = kha_cont:get_name(CPid),
    Build2 = build_append(io_lib:format("# waiting for container ~s~n", [Name]), Build),
    kha_cont:wait(CPid),
    Build2.

container_stop(CPid, Build) ->
    {ok, Name} = kha_cont:get_name(CPid),
    Build2 = build_append(io_lib:format("# stopping container ~s~n", [Name]), Build),
    kha_cont:stop(CPid),
    build_append(io_lib:format("# container stopped ~s~n", [Name]), Build2).

ensure_build_dir(P, Build) ->
    Dir = case Build#build.dir of
              undefined ->
                  PN = kha_utils:clean_filename(P#project.name),
                  ["/tmp/kha/", PN, "/",
                   kha_utils:clean_filename(kha_utils:now_to_nice(now())), "-", hex:to(crypto:rand_bytes(2)), "/",
                   PN];
              D -> D
          end,
    Build#build{dir = kha_utils:convert(Dir, str)}.


do_process({ProjectId, BuildId}, ContData) ->
    {ok, P} = kha_project:get(ProjectId),
    {ok, Build0} = kha_build:get(ProjectId, BuildId),
    Build = ensure_build_dir(P, Build0#build{status = building}),
    kha_build:update(Build),
    kha_hooks:run(on_building, Build),

    Config = case kha_config:fetch(P, Build) of
                 {ok, [C]} ->
                     C;
                 {error, _} ->
                     []
             end,

    ProjectSteps = get_project_build_script(P),
    Steps0 = case proplists:get_value(<<"script">>, Config) of
                 undefined ->
                     ProjectSteps;
                 Scr ->
                     [Scr]
             end,
    Before = fetch_steps([<<"before_install">>,
                          <<"install">>,
                          <<"after_install">>,
                          <<"before_script">>], Config),
    After = fetch_steps([<<"after_success">>, %% there's no support for after_failure yet
                         <<"after_script">>], Config),
    CloneSteps = create_clone_steps(P, Build),
    Steps = lists:concat([CloneSteps,
                          Before,
                          Steps0,
                          After]),

    Envs = kha_lang:get_envs(Config),

    BuildFinal =
        lists:foldl(fun(Env, BB) ->
                            execute_job(Env, ContData, Steps, P, BB)
                    end, Build, Envs),

    case BuildFinal#build.status of
        success -> kha_hooks:run(on_success, BuildFinal);
        fail    -> kha_hooks:run(on_failed,  BuildFinal);
        timeout -> kha_hooks:run(on_failed,  BuildFinal)
    end,
    kha_notification:run(P, BuildFinal).

execute_job(Env, ContData, BuildSteps,
            #project{id = ProjectId} = P,
            #build{id = BuildId} = Build0) ->
    Timeout = proplists:get_value(<<"build_timeout">>, P#project.params, 60),
    {ok, Container} = container_start(ContData),
    BB2 = container_wait(Container, Build0),
    {ok, Timer} = set_timeout(Timeout, {?MODULE, build_timeout, [self(), ProjectId, BuildId]}),
    EnvSteps = create_env_steps(Env),
    Steps = stepify(Container, BB2, P, lists:concat([EnvSteps,
                                                     BuildSteps])),
    BB3 = finalize_build(lists:foldl(fun process_step/2, BB2, Steps)),
    cancel_timeout(Timer),
    kha_build:update(BB3),
    container_stop(Container, BB3),
    BB3.

create_env_steps([]) ->
    [];
create_env_steps([{env, E} | Env]) ->
    [ [ iolist_to_binary(["export ", K, "=\"", V, "\""]) || {K, V} <- E ] | create_env_steps(Env) ];
create_env_steps([{Lang, Ver} | Env]) ->
    [ kha_lang:set_runtime_cmd(Lang, Ver) | create_env_steps(Env) ].


finalize_build(Build) ->
    case Build of
        #build{status = building} = BS ->
            BS#build{status = success,
                     stop = now(),
                     exit = 0};
        #build{status = fail} = BF ->
            BF
    end.

fetch_steps(Params, Config) ->
    lists:flatmap(fun(P) ->
                          fetch_step(P, Config)
                  end, Params).

fetch_step(Param, Config) ->
    case proplists:get_value(Param, Config, []) of
        [] ->
            [];
        [L|_] = X when is_list(L) ->
            lists:map(fun iolist_to_binary/1, X);
        L when is_list(L) ->
            [iolist_to_binary(L)]
    end.

create_clone_steps(P, Build) ->
    Dir = Build#build.dir,
    Remote = kha_utils:convert(P#project.remote, str),
    Rev = kha_build:get_rev(Build),
    [ {io_lib:format("[ -d \"~s\" ] || ~s", [Dir, git:clone_cmd(Remote, Dir, [])]), []},
      git:fetch_cmd(Dir),
      git:checkout_cmd(Dir, Rev, [force]) ].

get_project_build_script(P) ->
    P#project.build.

stepify(Container, B, P, Steps0) ->
    lists:flatmap(fun(Command) ->
                          stepify0(Container, B, P, Command)
                  end, Steps0).

stepify0(_Container, _B, _P, {"", _Opts}) ->
    [];
stepify0(Container, _B, _P, {Command, Opts}) ->
    [{Command,
      fun(Ref, Parent) ->
              kha_cont:exec_stream(Container, Command, Ref, Parent, Opts)
      end}];
stepify0(Container, B, P, Command) ->
    Dir = kha_utils:convert(B#build.dir, str),
    stepify0(Container, B, P, {Command, [{cd, Dir}]}).


process_step(_, #build{status = X} = B) when X /= building ->
    B;
process_step({Cmd, F}, B) ->
    B2 = build_append(io_lib:format("$ ~s~n", [Cmd]), B),
    Parent = self(),
    Ref = make_ref(),
    proc_lib:spawn_link(fun() ->
                                Res = F(Ref, Parent),
                                Parent ! {Ref, done, Res}
                        end),
    process_loop(Ref, Parent, Cmd, B2).

process_loop(Ref, Parent, Cmd, Build) ->
    receive
        {Ref, done, Res} ->
            case Res of
                {ok, D} ->
                    build_append(D, Build);
                {error, {ExitCode, Reason}} ->
                    Build#build{output = [io_lib:format("# exit code: ~b~n", [ExitCode]),
                                          Reason
                                          | Build#build.output],
                                stop = now(),
                                exit = ExitCode,
                                status = fail}
            end;
        {Ref, line, Line} ->
            B2 = build_append(Line, Build),
            process_loop(Ref, Parent, Cmd, B2)
    end.

build_append(Line, #build{output = Output0} = Build) ->
    B2 = Build#build{output = [ Line | Output0 ]},
    kha_build:update(B2),
    B2.

set_timeout(Time, {M, F, A}) ->
    timer:apply_after(timer:seconds(Time), M, F, A).

cancel_timeout(Timer) ->
    timer:cancel(Timer).
