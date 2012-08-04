%% @doc Simple GIT client.
%%
%% WARNING: all calls are not safe!
%% @todo Add behaviour
%% @todo Escape input

%% @author Martynas <martynasp@gmail.com>

-module(kha_git).

-export([
         clone/2,
         check_for_updates/3,
         checkout/2
        ]).

-export([clone_cmd/2, checkout_cmd/1]).

-define(FMT(Msg, Args), lists:flatten(io_lib:format(Msg, Args))).

%% ============================================================================

%% @throws {unable_to_clone, Reason :: list()}>
-spec clone(list(), list()) -> ok.
clone(RepoURL, RepoPath) ->
    %% kha_utils:sh(?FMT("rm -rf \"~s\"", [RepoPath])), % dirty hack
    try
        Output = kha_utils:sh(clone_cmd(RepoURL, RepoPath)),
        {ok, [Output]}
    catch
        {exec_error, {_, _, Reason}} ->
            throw({unable_to_clone, Reason})
    end.

clone_cmd(RepoURL, RepoPath) ->
    ?FMT("git clone \"~s\" \"~s\"", [RepoURL, RepoPath]).

%% @doc Checks if for given repo's branch there exists a "newer" commit than
%% given commit.
-spec check_for_updates(perforator_ci_types:repo_path(),
                        perforator_ci_types:branch(), perforator_ci_types:commit_id()) ->
                               perforator_ci_types:commit_id() | undefined.
check_for_updates(RepoPath, Branch, CommitID) ->
    try
        kha_utils:sh("git fetch", [{cd, RepoPath}]),
        kha_utils:sh(?FMT("git checkout \"~s\"", [Branch]),
                     [{cd, RepoPath}]),

        CommitID1 = list_to_binary(
                      lists:reverse(
                        tl(tl(lists:reverse(
                                kha_utils:sh(
                                  ?FMT("git log -n 1 --format=\"%H\"", []),
                                  [{cd, RepoPath}])
                               )))
                       )
                     ),

        %% check if returned commit id is ancestor of previous commit
        case CommitID of
            undefined -> CommitID1;
            _ ->
                case kha_utils:sh(?FMT("git log ~s..~s",
                                       [CommitID, CommitID1]), [{cd, RepoPath}]) of
                    [] -> % nope
                        undefined;
                    _ -> % yes
                        CommitID1
                end
        end
    catch
        throw:{exec_error, {_, 128, _}} -> % 128 most likely git repo is empty
            undefined;
        throw:{exec_error, {_, 1, _}} -> % 1 most likely branch not found
            undefined
    end.

%% @doc Fetches changes and tries to checkout to given commit.
%% @throws {unable_to_checkout, Reason}
-spec checkout(list(), perforator_ci_types:commit_id()) -> ok.
checkout(RepoDir, CommitID) ->
    try
        Output = kha_utils:sh(checkout_cmd(CommitID), [{cd, RepoDir}]),
        {ok, Output}
    catch
        throw:{exec_error, {_, 128, Reason}} ->
            throw({unable_to_checkout, Reason})
    end.

checkout_cmd(CommitID) ->
    ?FMT("git fetch && git checkout ~s", [CommitID]).
