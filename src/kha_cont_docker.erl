%%%-------------------------------------------------------------------
%%% @author  <gleber@first.lan>
%%% @copyright (C) 2012,
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2012 by  <gleber@first.lan>
%%%-------------------------------------------------------------------
-module(kha_cont_docker).

-behaviour(gen_server).

%% API
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("common.hrl").
-include("kha.hrl").

-define(s, State#state).

-record(state, {image,
                name,
                runner = undefined,
                opts = []}).

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
start(Image, Opts) ->
    gen_server:start(?MODULE, [Image, Opts], []).

start_link(Image, Opts) ->
    gen_server:start_link(?MODULE, [Image, Opts], []).

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
init([Image, Opts]) ->
    process_flag(trap_exit, true),
    {ok, #state{image = Image,
                opts = Opts}, 0}.

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
handle_call(wait, _From, #state{} = State) ->
    {reply, true, State};

handle_call(get_name, _From, #state{} = State) ->
    {reply, {ok, ?s.name}, State};

handle_call({exec_stream, Command, Ref, Parent, Opts}, _From, #state{runner = Runner} = State) ->
    Res = runner:exec_stream_sync(Runner, Command, Ref, Parent, Opts),
    {reply, Res, State};

handle_call({exec, Command, _Opts}, _From, #state{runner = Runner} = State) ->
    Res = runner:exec_aggregate_sync(Runner, Command),
    {reply, Res, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {stop, {unknown_call, _Request}, {error, {unknown_call, _Request}}, State}.

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
handle_info({'EXIT', Runner, Reason}, #state{runner = Runner} = State) ->
    {stop, Reason, State};
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    ?LOG("Starting container ~s with opts ~p~n", [?s.image, ?s.opts]),
    {ok, Prefix} = docker:runner_str(?s.image, ?s.opts),
    {ok, R} = runner:spawn([{prefix, Prefix}]),
    {ok, Name} = runner:exec_aggregate_sync(R, "hostname"),
    {noreply, State#state{name = Name, runner = R}};

%% handle_info({eoc, Runner, 0, _}, #state{runner = Runner} = State) ->
%%     {noreply, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

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
terminate(_Reason, #state{runner = Runner} = _State) ->
    runner:stop(Runner),
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
