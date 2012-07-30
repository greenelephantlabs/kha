-module(kha_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(cowboy),
    application:start(kha).

start(_Type, _Args) ->
    Dispatch = [
                {'_', [
                       %% {[<<"article">>, '_', <<"message">>, '_'], message_handler, []},
                       %% {[<<"article">>, '_', <<"message">>], message_handler, []},
                       %% {[<<"article">>, '_'], article_handler, []},
                       %% {[<<"article">>], article_handler, []},

                       {[<<"builds">>], kha_build_handler, []},
                       {[<<"builds">>, build], kha_build_handler, []},
                       {'_', default_handler, []}
                      ]}
               ],
    cowboy:start_listener(spock_http_listener, 10,
                          cowboy_tcp_transport, [{port, 8093}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    kha_sup:start_link().

stop(_State) ->
    ok.