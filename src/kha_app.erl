-module(kha_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    db:init(),
    ok = application:start(crypto, permanent),
    ok = application:start(public_key, permanent),
    ok = application:start(inets, permanent),
    ok = application:start(ssl, permanent),
    ok = application:start(bcrypt, permanent),
    ok = application:start(yamerl, permanent),
    ok = application:start(ranch, permanent),
    ok = application:start(cowboy, permanent),
    ok = application:start(mimetypes, permanent),
    ok = application:start(kha, permanent).

start(_Type, _Args) ->       
    case validate_env(kha) of
        ok ->
            configure_cowboy(),
            kha_sup:start_link();
        {error, Error} ->
            {error, Error}
    end.

stop(_State) ->
    ok.

validate_env(kha) ->
    case application:get_env(kha, host) of
        X when X =:= undefined ->
            io:fwrite("Error! Application env 'host' is not defined.~n", []),
            {error, env_variable__host__is_undefined};
        _X -> 
            ok
    end.

configure_cowboy() ->    
    Dispatch = [
                {'_', [
                       %% PROJECT
                       {[<<"user">>, '_'], kha_user_handler, []},

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
    cowboy:start_http(kha_http_listener, 10, [{port, 8093}], [{dispatch, Dispatch}]).
