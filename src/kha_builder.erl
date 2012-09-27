%%%-------------------------------------------------------------------
%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% kha_builder module
%%% @end
%%% Created : 31 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>
%%%-------------------------------------------------------------------
-module(kha_builder).

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
    {ok, #state{queue = queue:new()}}.

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
            Pid = proc_lib:spawn(fun() -> do_process(Job) end),
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
handle_info({'DOWN', _, process, Pid, NormalOrTimeout}, #state{busy = {Pid, _}} = State) when NormalOrTimeout == normal;
                                                                                              NormalOrTimeout == timeout ->
    kha_builder:process(),
    {noreply, State#state{busy = false}};

handle_info({'DOWN', _, process, Pid, _Reason}, #state{busy = {Pid, Job}} = State) ->
    {ProjectId, BuildId} = Job,
    {ok, Build0} = kha_build:get(ProjectId, BuildId),
    kha_build:update(Build0#build{status = fail}),
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

build_timeout(Pid, ProjectId, BuildId) ->
    exit(Pid, timeout),
    {ok, Build0} = kha_build:get(ProjectId, BuildId),
    Build = Build0#build{status = timeout},
    kha_build:update(Build).

do_process({ProjectId, BuildId}) ->
    {ok, P} = kha_project:get(ProjectId),
    {ok, Build0} = kha_build:get(ProjectId, BuildId),
    Build = Build0#build{status = building},
    kha_build:update(Build),
    kha_hooks:run(on_building, ProjectId, BuildId),
    Local = kha_utils:convert(P#project.local, str),
    Remote = kha_utils:convert(P#project.remote, str),
    BuildTimeout = proplists:get_value(<<"build_timeout">>, P#project.params, 60),

    {ok, Timer} = set_timeout(BuildTimeout, {?MODULE, build_timeout, [self(), ProjectId, BuildId]}),

    UserSteps = get_user_steps(P, Build),
    CloneStep = create_clone_step(Local, Remote),

    Steps = [ CloneStep | UserSteps ],

    Build2 = try
                 B2 = lists:foldl(fun process_step/2, Build, Steps),
                 B2#build{status = success,
                          stop = now(),
                          exit = 0}
             catch
                 throw:{error, Bb} ->
                     Bb
             end,

    cancel_timeout(Timer),
    kha_build:update(Build2),
    case Build2#build.status of
        success -> kha_hooks:run(on_success,   ProjectId, BuildId);
        fail    -> kha_hooks:run(on_failed, ProjectId, BuildId);
        timeout -> kha_hooks:run(on_failed, ProjectId, BuildId)
    end,
    kha_notification:run(P, Build2),
    Build2.

create_clone_step(Local, Remote) ->
    case filelib:is_dir(Local) of
        true ->
            {"# no need to checkout\n",
             fun() -> {ok, ""} end};
        false ->
            {git:clone_cmd(Remote, Local, []),
             fun() -> kha_utils:sh(git:clone_cmd(Remote, Local, [])) end}
    end.    

get_user_steps(P, Build) ->
    Local = kha_utils:convert(P#project.local, str),
    Branch = kha_utils:convert(Build#build.branch, str),
    Revision = kha_utils:convert(Build#build.revision, str),
    Ref = case Revision of
              undefined -> Branch;
              "" -> Branch;
              _ -> Revision
          end,

    Steps0 = [ git:fetch_cmd(Local),
               git:checkout_cmd(Local, Ref, [force])
               | P#project.build ],

    UserSteps = [ {C, fun() -> kha_utils:sh(C, [{cd, Local}]) end} || C <- Steps0 ],
    UserSteps.

process_step({Cmd, F}, B) ->
    B2 = B#build{output = [io_lib:format("$ ~s~n", [Cmd]) | B#build.output]},
    kha_build:update(B2),
    case F() of
        {ok, D} ->
            B3 = B#build{output = [D | B2#build.output]},
            kha_build:update(B3),
            B3;
        {error, {ExitCode, Reason}} ->
            Be = B#build{output = [Reason,
                                   io_lib:format("$ ~s~n", [Cmd])
                                   | B#build.output],
                         stop = now(),
                         exit = ExitCode,
                         status = fail},
            throw({error, Be})
    end.

set_timeout(Time, {M, F, A}) ->
    timer:apply_after(timer:seconds(Time), M, F, A).

cancel_timeout(Timer) ->
    timer:cancel(Timer).
