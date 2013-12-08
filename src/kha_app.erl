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
    ok = application:ensure_started(sasl, permanent),
    ok = application:ensure_started(asn1, permanent),
    ok = application:ensure_started(crypto, permanent),
    ok = application:ensure_started(public_key, permanent),
    ok = application:ensure_started(bcrypt, permanent),
    ok = application:ensure_started(inets, permanent),
    ok = application:ensure_started(ssl, permanent),
    ok = application:ensure_started(yamerl, permanent),
    ok = application:ensure_started(cowlib, permanent),
    %% ok = application:load(ranch, permanent),
    %% ok = application:load(cowboy, permanent),
    ok = application:ensure_started(mimetypes, permanent), 
    {ok, _} = application:ensure_all_started(kha, permanent).

stop() ->
    ok = application:stop(kha).

start(_Type, _Args) ->
    case validate_env(kha) of
        ok ->
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
