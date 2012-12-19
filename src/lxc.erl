-module(lxc).

-compile(export_all).

start(Name) ->
    start(Name, []).

start(Name, Opts) ->
    case proplists:get_value(ephemeral, Opts, false) of
        true ->
            start_ephemeral(Name, Opts);
        false ->
            case lists:member(Name, list()) of
                true ->
                    Ref = sh:run(
                            lists:flatten(
                              io_lib:format("sudo lxc-start -n \"~s\"", [Name])), [{async, true}]),
                    {ok, Name, Ref};
                false ->
                    {error, not_found}
            end
    end.

start_ephemeral(Base) ->
    start_ephemeral(Base, []).

start_ephemeral(Base, _Opts) ->
    Containers = list(),
    case lists:member(Base, Containers) of
        true ->
            Ref = sh:run(
                    lists:flatten(
                      io_lib:format("sudo lxc-start-ephemeral -d -o \"~s\"", [Base])), [{async, true}]),    
            case wait_for_new(5, Containers) of
                {ok, [Name]} ->
                    {ok, Name, Ref};
                Error ->
                    Error
            end;
        false ->
            {error, not_found}
    end.

stop(Name) ->
    kha_utils:sh("sudo lxc-stop -n \"~s\"", [Name], []).

destroy(Name) ->
    kha_utils:sh("sudo lxc-destroy -n \"~s\"", [Name], []).    

list() ->
    %%[ X || "/var/lib/lxc/" ++ X <- lists:usort(filelib:wildcard("/var/lib/lxc/*")) ].
    {ok, O} = kha_utils:sh("sudo lxc-ls"),
    lists:usort(string:tokens(O, " \n")).

info(Name) ->
    {ok, O} = kha_utils:sh("sudo lxc-info -n \"~s\"", [Name], []),
    yamerl_constr:string(O).

wait_for_new() ->
    wait_for_new(5).

wait_for_new(Timeout) ->
    Containers = list(),
    wait_for_new(Timeout, Containers).

wait_for_new(0, _) ->
    {error, timeout};
wait_for_new(Timeout, Containers) ->
    Current = list(),
    case Containers of
        Current ->
            timer:sleep(1000),
            wait_for_new(Timeout - 1, Containers);
        _ ->
            {ok, Current -- Containers}
    end.


exec_prefix(Name) ->
    binary_to_list(iolist_to_binary(io_lib:format("lxc-ssh -n \"~s\" -- ", [Name]))).

exec_prefix(Name, Opts) ->
    case proplists:get_value(username, Opts) of
        undefined -> exec_prefix(Name);
        Username ->
            binary_to_list(iolist_to_binary(io_lib:format("lxc-ssh -n \"~s\" -u \"~s\" -- ", [Name, Username])))
    end.

exec(Name, Command, Args) ->
    exec(Name, [], Command, Args).

exec(Name, Opts, Command, Args) ->
    kha_utils:sh(io_lib:format("~s ~s", [exec_prefix(Name, Opts), Command]), Args, []).
