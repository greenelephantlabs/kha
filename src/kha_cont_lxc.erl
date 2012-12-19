%%%-------------------------------------------------------------------
%%% @author  <gleber@first.lan>
%%% @copyright (C) 2012,
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2012 by  <gleber@first.lan>
%%%-------------------------------------------------------------------
-module(kha_cont_lxc).

-behaviour(gen_server).

%% API
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

-define(s, State#state).

-record(state, {original_name,
                name,
                ref = undefined,
                opts = [],
                ready = false,
                waiters = ordsets:new()}).

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
init([Name, Opts]) ->
    process_flag(trap_exit, true),
    {ok, #state{original_name = Name,
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
handle_call({attach, Pid}, _From, State) ->
    link(Pid),
    {reply, ok, State};

handle_call(get_exec_prefix, _From, #state{ready = true} = State) ->
    Prefix = lxc:exec_prefix(?s.name, ?s.opts),
    {reply, {ok, Prefix}, State};
handle_call(get_exec_prefix, _From, #state{ready = false} = State) ->
    {reply, {error, not_ready}, State};

handle_call(get_name, _From, #state{} = State) ->
    {reply, {ok, ?s.name}, State};

handle_call(wait, _From, #state{ready = true} = State) ->
    {reply, true, State};
handle_call(wait, From, State) ->
    {noreply, ?s{waiters = ordsets:add_element(From, ?s.waiters)}};

handle_call({exec_stream, Command, Ref, Parent, Opts0}, _From, State) ->
    Prefix0 = lxc:exec_prefix(?s.name, ?s.opts),
    [Prefix, Postfix] = case proplists:get_value(cd, Opts0) of
                            undefined -> [Prefix0, ""];
                            X -> [[Prefix0, "'cd \"", X, "\" && "], "'"]
                        end,
    Opts = lists:keydelete(cd, 1, Opts0),
    Res = kha_utils:sh_stream([Prefix, Command, Postfix], Ref, Parent, Opts),
    {reply, Res, State};

handle_call({exec, Command, Opts0}, _From, State) ->
    Prefix0 = lxc:exec_prefix(?s.name, ?s.opts),
    [Prefix, Postfix] = case proplists:get_value(cd, Opts0) of
                            undefined -> [Prefix0, ""];
                            X -> [[Prefix0, "'cd \"", X, "\" && "], "'"]
                        end,
    Opts = lists:keydelete(cd, 1, Opts0),
    Res = kha_utils:sh([Prefix, Command, Postfix], Opts),
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
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    ?LOG("Starting container ~s with opts ~p~n", [?s.original_name, ?s.opts]),
    {ok, Name, Ref} = lxc:start(?s.original_name, ?s.opts),
    timer:send_after(1000, do_ping),
    {noreply, State#state{name = Name, ref = Ref}};

handle_info(do_ping, State) ->
    case lxc:exec(?s.name, ?s.opts, "uname -a", []) of
        {ok, _} ->
            [ gen_server:reply(X, true) || X <- ordsets:to_list(?s.waiters) ],
            {noreply, State#state{waiters = undefined, ready = true}};
        _ ->
            timer:send_after(1000, do_ping),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

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
terminate(_Reason, State) ->
    lxc:stop(?s.name),
    sh:join(?s.ref),
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
