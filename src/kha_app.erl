-module(kha_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

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
                       %% USER
                       {"/user/[:user_id]", kha_user_handler, []},

                       %% PROJECT
                       {"/project",                kha_project_handler, []},
                       {"/project/[:project_id]",  kha_project_handler, []},

                       %% BUILD
                       {"/project/[:project_id]/build",                   kha_build_handler, []},
                       {"/project/[:project_id]/build/[:build_id]",       kha_build_handler, []},
                       {"/project/[:project_id]/build/[:build_id]/stop",  kha_build_handler, []},

                       %% HOOKS
                       {"/hooks/[:type]/[:project_id]", kha_hooks_handler, []},

                       %% DEFAULT
                       {"/", cowboy_static, [{directory, {priv_dir, kha_app, <<"www">>}},
                                             {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
                                             {file, <<"index.html">>}
                                            ]},

                       {"/[...]", cowboy_static, [{directory, {priv_dir, kha_app, <<"www">>}},
                                                  {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                                                 ]}
                      ]}
               ],
    cowboy:start_http(kha_http_listener, 10, [{port, 8093}],
                      [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}
                      ]),
    ?LOG("Finish configure cowboy...", []).
