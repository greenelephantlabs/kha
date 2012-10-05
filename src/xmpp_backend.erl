%%%-------------------------------------------------------------------
%%% @author Paul Peregud <paulperegud@gmail.com>
%%% @copyright (C) 2012, Paul Peregud
%%% @doc
%%%
%%% Keeps persistent xmpp connection to server, handles user commands 
%%% and delivers notifications.
%%%
%%% Note: some of this code is a was copied from 'echo_client.erl' 
%%% example of exmpp
%%%
%%% @end
%%% Created :  4 Oct 2012 by Paul Peregud <paulperegud@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_backend).

-behaviour(gen_server).

%% API
-export([send/5]).

-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/kha.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(SERVER, ?MODULE). 

-type opaque() :: any().

-record(mute, {
          id :: {binary(), non_neg_integer()}, %% for easy indexing
          uid :: binary(),
          project_id :: non_neg_integer(),
          until = kha_utils:now_sum(now(), timer:hours(8)) :: timer:timestamp()
         }).

-record(state, {
          session :: opaque(),
          bot_jid :: binary(),
          mutes = [] :: [#mute{}],
          mutes_expire_timer :: timer:tref()
         }).

-define(s, State#state).

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
send(Pid, Project, Title, Content, Args) ->
    gen_server:cast(Pid, {send, Project, Title, Content, Args}).

start_link(Username, Server, Pass) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Username, Server, Pass], []).

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
init([JID, Server, Password]) ->
    application:start(exmpp),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    [User, Domain] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, Domain, random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, Server, 5222),
    session(MySession, JID).

%% We are connected. We now log in.
session(MySession, JID) ->
    %% Login with defined JID / Authentication:
    exmpp_session:login(MySession), %% if fails - crash
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "bot: write me help")),
    {ok, Timer} = timer:send_interval(timer:minutes(1), check_mutes),
    {ok, #state{session = MySession, bot_jid = JID, mutes_expire_timer = Timer}}.

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
handle_call(_Request, _From, State) ->
    {stop, {odd_call, _Request, _From}, State}.

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
handle_cast({send, Project, Title, Contents, Args}, State) ->
    P1 = template_packet(Title, Contents, Args, ?s.bot_jid),
    F = fun(To) ->
                P2 = exmpp_xml:set_attribute(P1, <<"to">>, To),
                exmpp_session:send_packet(?s.session, P2)
        end,
    [ F(Mail) || Mail <- filter_mutes(Project, Args, State) ],
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

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
handle_info(Record = #received_packet{packet_type = 'presence'}, State) ->
    ?LOG("Received Presence stanza:~n~p~nState: ~p", [Record, State]),
    handle_presence(Record, Record#received_packet.raw_packet, State),
    {noreply, State};

handle_info(Record = #received_packet{packet_type=message,
				  raw_packet=_Packet, type_attr=Type}, 
            State) when Type =/= "error" ->
    %% ?LOG("Received Message stanza:~n~p~n~nState: ~p~n", [Record, State]),
    State1 = maybe_handle_command(Record, State),
    {noreply, State1};

handle_info(#received_packet{packet_type = T} = Packet, State) ->
    ?LOG("ignoring ~p packet~ncontents: ~p", [T, Packet]),
    {noreply, State};

handle_info(check_mutes, State) ->
    Now = now(),
    NM = lists:filter(fun(#mute{until = X}) -> X > Now end, ?s.mutes),
    {noreply, ?s{mutes = NM}};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

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
    _ = timer:cancel(?s.mutes_expire_timer),
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

handle_presence(Packet, _Presence, State) ->
    JID = exmpp_jid:make(_From = Packet#received_packet.from),
    case _Type = Packet#received_packet.type_attr of
        "available" ->
            %% handle presence availabl
            ok;
        "unavailable" ->
            %% handle presence unavailable
            ok;
        "subscribe" ->
            presence_subscribed(JID, State),
            presence_subscribe(JID, State);
        "subscribed" ->
            presence_subscribed(JID, State),
            presence_subscribe(JID, State)
    end.

presence_subscribed(Recipient, State) ->
    Session = ?s.session,
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Recipient, State) ->
    Session = ?s.session,
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).

maybe_handle_command(#received_packet{raw_packet = Packet, from = To0}, State) ->
    To = to_jid(To0),
    Body = exmpp_message:get_body(Packet),
    ?LOG("from:~p~nbody: ~p", [To, Body]),
    case Body of
        <<"projects", _/binary>> -> 
            send_text(projects_text(), To, State);
        <<"mute">> -> 
            maybe_mute(all, To, State);
        <<"mute ", ProjectId/binary>> -> 
            maybe_mute(ProjectId, To, State);
        <<"help", _/binary>> -> 
            send_text(help_text(), To, State);
        _ -> 
            send_text(help_text(), To, State)
    end.

send_text(Text, To, State) ->
    Bin = list_to_binary(Text),
    P0 = exmpp_message:normal(Bin),
    P1 = exmpp_xml:set_attribute(P0, <<"from">>, ?s.bot_jid),
    P2 = exmpp_xml:set_attribute(P1, <<"to">>, To),
    exmpp_session:send_packet(?s.session, P2),
    State.

maybe_mute(all, To, State) ->
    {ok, Ps} = kha_project:get(all),
    NewMutes = [ #mute{id = {To, Id}, uid = To, project_id = Id} || #project{id = Id} <- Ps ],
    State1 = add_mutes(NewMutes, State),
    send_text(text("All notifications are muted for next 8 hours~n"), To, State1);
maybe_mute(Binary, To, State) when is_binary(Binary) ->
    case parse_int(Binary) of
        undefined ->
            send_text(text("Can't parse project number~n"), To, State);
        Num ->
            maybe_mute(Num, To, State)
    end;
maybe_mute(Id, To, State) when is_number(Id) ->
    case kha_project:get(Id) of
        {error, not_found} ->
            send_text(text("Project not found.~n"), To, State);
        {ok, #project{name = Name}} ->
            NewMutes = [ #mute{id = {To, Id}, uid = To, project_id = Id} ],
            State1 = add_mutes(NewMutes, State),
            send_text(io_lib:fwrite("Notifications for project ~p are muted for next 8 hours.~n", [Name]), To, State1)
    end.
    
add_mutes(New, State) ->
    Mutes = lists:foldl(fun(X, Acc) -> lists:keystore(X#mute.id, #mute.id, Acc, X) end, ?s.mutes, New),
    ?s{mutes = Mutes}.
    
filter_mutes(Project, List, State) ->
    Id = Project#project.id,
    Mutes = ?s.mutes,
    lists:filter(fun(Mail) -> false =:= lists:keyfind({Mail, Id}, #mute.id, Mutes) end, List).

template_packet(Title, Contents, _Args, JID) ->
    Bin = format_packet(Title, Contents),
    P0 = exmpp_message:normal(Bin),
    exmpp_xml:set_attribute(P0, <<"from">>, JID).

format_packet(Title, Contents) ->
    list_to_binary(lists:flatten([Title, Contents, cr(), delimiter(), Title, cr(), delimiter(), delimiter()])).

delimiter() ->
    io_lib:fwrite("==================================~n", []).

cr() ->
    io_lib:fwrite("~n", []).

projects_text() ->
    {ok, Ps} = kha_project:get(all),
    Projects = lists:flatten( [ io_lib:fwrite("#~p : ~p~n", [Id, Name]) || #project{id = Id, name = Name} <- Ps ] ),
    io_lib:fwrite("~nprojects:~n~s", [Projects]).

help_text() ->
    text("~navailable commands:~n- projects~n- mute [ID]~n- help~n").

text(Text) ->
    io_lib:fwrite(Text, []).

to_jid({User, Domain, _Resource}) ->
    <<User/binary, "@", Domain/binary>>.

parse_int(Binary) ->
    try 
        list_to_integer(binary_to_list(Binary))
    catch _:_ ->
            undefined 
    end.
    
