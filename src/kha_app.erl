-module(kha_app).

-behaviour(application).

-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

-include("common.hrl").
-include("kha.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    db:init(),
    ok = application:ensure_started(asn1, permanent),
    ok = application:ensure_started(crypto, permanent),
    ok = application:ensure_started(public_key, permanent),
    ok = application:ensure_started(bcrypt, permanent),
    ok = application:ensure_started(inets, permanent),
    ok = application:ensure_started(ssl, permanent),
    ok = application:ensure_started(yamerl, permanent),
    ok = application:ensure_started(cowlib, permanent),
    ok = application:ensure_started(ranch, permanent),
    ok = application:ensure_started(cowboy, permanent),
    ok = application:ensure_started(mimetypes, permanent),
    ok = application:ensure_started(kha, permanent).

stop() ->
    ok = application:stop(kha).

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
                       {"/project/[:project_id]/build",              kha_build_handler, []},
                       {"/project/[:project_id]/build/[:build_id]",  kha_build_handler, []},

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
