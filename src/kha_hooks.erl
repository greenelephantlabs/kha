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

-define(GLOBAL_HOOKS_LOCATION, <<"support/hooks/">>).
-define(PROJECT_HOOKS_LOCATION, <<"kha_hooks/">>).

hookfn(on_success)  -> <<"on_success">>;
hookfn(on_failed)   -> <<"on_failed">>;
hookfn(on_building) -> <<"on_building">>;
hookfn(on_timeout)  -> <<"on_timeout">>.

run(Which, ProjectId, BuildId) ->
    {ok, P} = kha_project:get(ProjectId),
    {ok, B} = kha_build:get(ProjectId, BuildId),
    %%run global hook
    SelfPath = kha_utils:get_app_path(),
    HookPath = filename:join([SelfPath, ?GLOBAL_HOOKS_LOCATION, hookfn(Which)]),
    do_run(Which, HookPath, P, B),

    %%run project hook
    Local = kha_utils:convert(P#project.local, str),
    LocalHookPath = filename:join([Local, ?PROJECT_HOOKS_LOCATION, hookfn(Which)]),
    do_run(Which, LocalHookPath, P, B).    

do_run(Which, HookPath, P, B) ->
    case filelib:is_file(HookPath) of
        true ->
            Opts = io_lib:fwrite("~b ~b \"~s\" \"~s\" \"~s\" \"~s\" \"~s\"",
                                 [P#project.id, B#build.id, B#build.title,
                                  B#build.branch, B#build.revision,
                                  B#build.author, P#project.remote]),
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
                 [Which, HookPath])
    end.
