-type project_id() :: integer().
-type build_id()   :: integer().
-type time()       :: {integer(), integer(), integer()}.
-type tag()        :: string().
-type command()    :: string().

-record(notification,
        {type          :: atom(), %% kha_notification_[type].erl
         params        :: proplists:proplist()
        }).

-record(project,
        {id            :: project_id(),
         name          :: string(),
         local         :: filename:filename(), %% git clone remote local
         remote        :: string(),
         build         :: list(command()),
         notifications :: list(#notification{})
        }).

-record(build,
        {key           :: {build_id(), project_id()},
         id            :: build_id(),
         project       :: project_id(),
         title         :: string() | 'undefined',
         branch        :: string(),
         revision      :: string(),
         author        :: string(),
         start         :: time(),
         stop          :: time(),
         exit          :: integer(),
         output        :: list(string()),
         tags          :: list(tag())
        }).

%%FIXME: PF: A temporary solution -> should by replace by alog or lager
-define('LOG', fun(X,Y) ->
                     Z = io_lib:fwrite("LOG[~p]: ~p~n", [?MODULE, X]),
                     io:fwrite(Z, Y)
               end).
