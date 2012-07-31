-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).


init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path0, _} = cowboy_http_req:path(Req),
    Path = case Path0 of
               [] -> ["index.html"];
               _ -> Path0
           end,
    FilePath = filename:join(Path),
    {ok, Cwd} = file:get_cwd(),
    File = filename:join([Cwd, "priv", "www", FilePath]),
    %% io:format("Request to file: ~p~n", [File]),
    %% NOTE - THERE IS A SECURITY HOLE HERE!
    case file:read_file(File) of
        {ok, Data} ->
            {ok, Req1} = cowboy_http_req:reply(200, [{<<"Content-Type">>, mimetypes:filename(File)}],
                                               Data, Req),
            {ok, Req1, State};
        {error, Reason} ->
            io:format("Cant read file ~p. Reason: ~p~n", [File, Reason]),
            {ok, Req1} = cowboy_http_req:reply(404, [{<<"Content-Type">>, "text/html"}],
                                               <<"404: File non found.">>, Req),
            {ok, Req1, State}
    end.

terminate(_Req, _State) ->
    ok.

