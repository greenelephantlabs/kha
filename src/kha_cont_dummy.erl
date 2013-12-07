%%%-------------------------------------------------------------------
%%% @author  <gleber@first.lan>
%%% @copyright (C) 2012,
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2012 by  <gleber@first.lan>
%%%-------------------------------------------------------------------
-module(kha_cont_dummy).

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

-record(state, {name = "dummy", runner}).

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
start(Name, Opts) ->
    gen_server:start(?MODULE, [Name, Opts], []).

start_link(Name, Opts) ->
    gen_server:start_link(?MODULE, [Name, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, _]) ->
    R = runner:spawn([]),
    {ok, #state{name = Name, runner = R}}.


handle_call(get_name, _From, State) ->
    {reply, {ok, ?s.name}, State};

handle_call(wait, _From, State) ->
    {reply, true, State};

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



handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
