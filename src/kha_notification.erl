-module(kha_notification).

-include("common.hrl").
-include("kha.hrl").

-export([run/2, type_to_module/1]).


run(Project, Build) ->
    Notif = Project#project.notifications,
    PId = Project#project.id,
    TitleProject = Project#project.name,
    BId = Build#build.id,
    Status = Build#build.status,
    Output = Build#build.output,
    {ok, Host} = application:get_env(kha, host),
    More = io_lib:format("Start: ~s~n"
                         "Stop: ~s~n"
                         "Author: ~s~n"
                         "Branch: ~s~n"
                         "Revision: ~s~n"
                         "Exit code: ~b~n"
                         "Url: http://~s/#/project/~b/build/~b~n~n",
                         [kha_utils:now_to_nice(Build#build.start),
                          kha_utils:now_to_nice(Build#build.stop),
                          Build#build.author, Build#build.branch,
                          Build#build.revision, Build#build.exit,
                          %% Create url
                          Host, PId, BId]),
    
    Title = io_lib:format("[~p] Project: ~b (~s); Build: ~b",
                          [Status, PId, TitleProject, BId]),
    [ begin
          ModuleName = type_to_module(Type),
          Content = [ More | lists:reverse(Output)],
          do_run(ModuleName, Project, Title, Content, Params)
      end || #notification{type = Type, params = Params} <- Notif ].

do_run(ModuleName, Project, Title, Content, Params) ->
    ModuleName:send(Project, Title, Content, Params).

type_to_module(Type) ->
    Name = io_lib:format("kha_notification_~p", [Type]),
    list_to_atom(lists:flatten(Name)).
