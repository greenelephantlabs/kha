-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/3]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path0, _} = cowboy_req:path(Req),
    Path = case Path0 of
               <<"/">> -> <<"/index.html">>;
               <<"/", Rest/binary>> -> Rest
           end,
    FilePath = Path,
    WwwPath = list_to_binary(get_priv_path("kha") ++ "/www/"),
    File = filename:join([WwwPath, FilePath]),
    case file:read_file(File) of
        {ok, Data} ->
            {ok, Req1} = cowboy_req:reply(200, [{<<"Content-Type">>, mimetypes:filename(File)}],
                                               Data, Req),
            {ok, Req1, State};
        {error, Reason} ->
            io:format("Cant read file ~p. Reason: ~p~n", [File, Reason]),
            {ok, Req1} = cowboy_req:reply(404, [{<<"Content-Type">>, "text/html"}],
                                               <<"404: File non found.">>, Req),
            {ok, Req1, State}
    end.

terminate(_,_,_) ->
    ok.

get_priv_path(App) ->
    AppFile = App++".app",
    FilePath = code:where_is_file(AppFile),
    FilePath2 = filename:dirname(filename:absname(FilePath)),
    filename:join([FilePath2, "..", "priv"]).
