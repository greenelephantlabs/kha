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
         fetch/1,
         checkout/2,

         remote_branches/1
        ]).

-export([clone_cmd/2,
         fetch_cmd/1,
         checkout_cmd/2]).

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

%% @doc Fetches recent changes from repo.
%% @throws {unable_to_checkout, Reason}
-spec fetch(list()) -> ok.
fetch(RepoDir) ->
    try
        Output = kha_utils:sh(fetch_cmd(RepoDir), [{cd, RepoDir}]),
        {ok, Output}
    catch
        throw:{exec_error, {_, 128, Reason}} ->
            throw({unable_to_fetch, Reason})
    end.

fetch_cmd(_RepoDir) ->
    "git fetch".

%% @doc Tries to checkout to given commit.
%% @throws {unable_to_checkout, Reason}
-spec checkout(list(), perforator_ci_types:commit_id()) -> ok.
checkout(RepoDir, CommitID) ->
    try
        Output = kha_utils:sh(checkout_cmd(RepoDir, CommitID), [{cd, RepoDir}]),
        {ok, Output}
    catch
        throw:{exec_error, {_, 128, Reason}} ->
            throw({unable_to_checkout, Reason})
    end.

checkout_cmd(_RepoDir, CommitID) ->
    ?FMT("git checkout -f ~s", [CommitID]).



-spec remote_branches(list()) -> ok.
remote_branches(Repo) ->
    try
        Output = kha_utils:sh(remote_branches_cmd(Repo), []),
        Branches = lists:map(fun(L) ->
                                     [Commit, Ref] = string:tokens(L, "\t "),
                                     {Type, Name} = case string:tokens(Ref, "/") of
                                                        ["refs", T | N] ->
                                                            {T, string:join(N, "/")};
                                                        ["HEAD"] ->
                                                            {"HEAD", "HEAD"}
                                                    end,
                                     {Commit, kha_utils:convert(string:strip(Type, right, $s), atom), Name}
                             end, string:tokens(Output, [13,10])),
        {ok, Branches}
    catch
        throw:{exec_error, {_, 128, Reason}} ->
            throw({unable_to_fetch_remote_branches, Reason})
    end.

remote_branches_cmd(Repo) ->
    ?FMT("git ls-remote ~s", [Repo]).
