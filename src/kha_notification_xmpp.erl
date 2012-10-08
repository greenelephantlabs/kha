%%%-------------------------------------------------------------------
%%% @author Paul Peregud <paulperegud@gmail.com>
%%% @copyright (C) 2012, Paul Peregud
%%% @doc
%%%
%%% Does needed formatting and sends notification
%%%
%%% @end
%%% Created :  3 Oct 2012 by Paul Peregud <paulperegud@gmail.com>
%%%-------------------------------------------------------------------
-module(kha_notification_xmpp).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

%% API
-export([send/4]).

%%%===================================================================
%%% API
%%%===================================================================

send(Project, Title, Content, Args) ->
    xmpp_backend:send(xmpp_backend, Project, Title, Content, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================
