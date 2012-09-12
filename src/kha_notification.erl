-module(kha_notification).

-include("kha.hrl").

-export([run/2, type_to_module/1]).


run(Project, Build) ->
    Notif = Project#project.notifications,
    PId = Project#project.id,
    BId = Build#build.id,
    Status = Build#build.status,
    Output = Build#build.output,
    
    Title = io_lib:format("Project: ~b; Build: ~b - ~p", [PId, BId, Status]),
    [ begin
          ModuleName = type_to_module(Type),
          do_run(ModuleName, Title, Output, Params)
      end || #notification{type = Type, params = Params} <- Notif ].

do_run(ModuleName, Title, Content, Params) ->
    ModuleName:send(Title, Content, Params).

type_to_module(Type) ->
    Name = io_lib:format("kha_notification_~p", [Type]),
    list_to_atom(lists:flatten(Name)).
