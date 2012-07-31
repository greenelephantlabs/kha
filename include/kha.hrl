%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @author Gleb Peregud <gleber.p@gmail.com>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% RECORD DEFINITIONS
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

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
         status        :: 'pending' | 'building' | 'succeed' | 'failed',
         exit          :: integer(),
         output        :: list(string()),
         tags          :: list(tag())
        }).

-record(id_seq,
        {whose      :: atom(),
         id = 10000 :: integer()
        }).

%%FIXME: PF: A temporary solution -> should by replace by alog or lager
-define(LOG(X,Y), (fun() ->
                           %% Z = io_lib:fwrite("LOG[~p]: ~p~n", [?MODULE, X]),
                           io:fwrite(X, Y)
                   end)()).
