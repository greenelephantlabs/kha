-module(lxc).

-compile(export_all).

start(Name) ->
    start(Name, []).

start(Name, Opts) ->
    case proplists:get_value(ephemeral, Opts, false) of
        true ->
            start_ephemeral(Name, Opts);
        false ->
            Ref = sh:run(
                    lists:flatten(
                      io_lib:format("sudo lxc-start -n \"~s\"", [Name])), [{async, true}]),
            {ok, Name, Ref}
    end.

start_ephemeral(Base) ->
    start_ephemeral(Base, []).

start_ephemeral(Base, _Opts) ->
    Containers = list(),
    Ref = sh:run(
            lists:flatten(
              io_lib:format("sudo lxc-start-ephemeral -d -o \"~s\"", [Base])), [{async, true}]),    
    [Name] = wait_for_new(Containers),
    {ok, Name, Ref}.

stop(Name) ->
    kha_utils:sh("sudo lxc-stop -n \"~s\"", [Name], []).

list() ->
    %%[ X || "/var/lib/lxc/" ++ X <- lists:usort(filelib:wildcard("/var/lib/lxc/*")) ].
    {ok, O} = kha_utils:sh("sudo lxc-ls"),
    lists:usort(string:tokens(O, " \n")).

info(Name) ->
    {ok, O} = kha_utils:sh("sudo lxc-info -n \"~s\"", [Name], []),
    yamerl_constr:string(O).

wait_for_new() ->
    Containers = list(),
    wait_for_new(Containers).

wait_for_new(Containers) ->
    Current = list(),
    case Containers of
        Current ->
            timer:sleep(1000),
            wait_for_new(Containers);
        _ ->
            Current -- Containers
    end.


wait_for_stop(Name) ->
    Containers = list(),
    case lists:member(Name, Containers) of
        Containers ->
            timer:sleep(1000),
            wait_for_new(Containers);
        Current ->
            Current -- [Containers]
    end.


exec_prefix(Name) ->
    binary_to_list(iolist_to_binary(io_lib:format("lxc-ssh -n \"~s\" --", [Name]))).

exec_prefix(Name, undefined) ->
    exec_prefix(Name);
exec_prefix(Name, Username) ->
    binary_to_list(iolist_to_binary(io_lib:format("lxc-ssh -n \"~s\" -u \"~s\" --", [Name, Username]))).

exec(Name, Command, Args) ->
    kha_utils:sh(io_lib:format("~s ~s", [exec_prefix(Name), Command]), Args, []).

exec(Name, undefined, Command, Args) ->
    exec(Name, Command, Args);
exec(Name, Username, Command, Args) ->
    kha_utils:sh(io_lib:format("~s ~s", [exec_prefix(Name, Username), Command]), Args, []).
