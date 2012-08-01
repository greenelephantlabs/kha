%%%-------------------------------------------------------------------
%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% kha_hooks module
%%% @end
%%% Created : 01 Aug 2012 by Paul Peter Flis <pawel@flycode.pl>
%%%-------------------------------------------------------------------
-module(kha_hooks).

-include("kha.hrl").

-export([run/3]).

-define(HOOKS_LOCATION, <<"support/hooks/">>).
-define(HOOK(Name), (fun() ->
                            case Name of
                                on_success -> <<"on_success">>;
                                on_failed  -> <<"on_failed">>;
                                on_building   -> <<"on_building">>;
                                on_timeout -> <<"on_timeout">>;
                                _ -> throw({error, undefined_hook})
                            end
                    end)()).

run(Which, ProjectId, BuildId) ->
    SelfPath = kha_utils:get_app_path(),
    HookPath = filename:join([SelfPath, ?HOOKS_LOCATION ,?HOOK(Which)]),

    case filelib:is_file(HookPath) of
        true ->
            {ok, P} = kha_project:get(ProjectId),
            {ok, B} = kha_build:get(ProjectId, BuildId),

            Opts = io_lib:fwrite("~b ~b \"~s\" \"~s\" \"~s\" \"~s\"",
                                 [ProjectId, BuildId, B#build.title,
                                  B#build.branch, B#build.author,
                                  P#project.remote]),
            Cmd = io_lib:fwrite("~s ~s", [HookPath, Opts]),
            try 
                kha_utils:sh(Cmd)
            catch
                throw:{exec_error, {_, ExitCode, Reason}} ->
                    ?LOG("Hook for ~p exit with code: ~b; Output: ~p",
                         [Which, ExitCode, Reason])
            end;
        false ->
            ?LOG("Hook for ~p not exist (not found ~s file)",
                 [Which, ?HOOK(Which)])
    end.
