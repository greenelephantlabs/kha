-module(kha_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(cowboy),
    ok = application:start(mimetypes),
    ok = application:start(kha).

start(_Type, _Args) ->
    case application:get_env(kha, host) of
        X when X =:= {ok, "example.host"}; X =:= undefined ->
            io:fwrite("!!!!~n\tEnv HOST is not defined - see src/kha.app.src~n!!!!!", []),
            timer:sleep(2000),
            init:stop(1);
        _X -> ok
    end,
        
    Dispatch = [
                {'_', [
                       %% PROJECT
                       {[<<"project">>], kha_project_handler, []},
                       {[<<"project">>, '_'], kha_project_handler, []},

                       %% BUILD
                       {[<<"project">>, '_', <<"build">>], kha_build_handler, []},
                       {[<<"project">>, '_', <<"build">>, '_'], kha_build_handler, []},

                       %% HOOKS
                       {[<<"hooks">>, '_', '_'], kha_hooks_handler, []},

                       %% DEFAULT
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
