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
-type b_string()   :: binary().

-record(notification,
        {type               :: atom(), %% kha_notification_[type].erl
         params             :: proplists:proplist()
        }).

-record(project,
        {id                 :: project_id(),
         server             :: pid(),
         name               :: b_string(),
         local              :: filename:filename(), %% git clone remote local
         remote             :: b_string(),
         build = []         :: list(command()),
         params = []        :: proplists:proplist(),
         notifications = [] :: list(#notification{})
        }).

-record(build,
        {key                :: {project_id(), build_id()},
         id                 :: build_id(),
         project            :: project_id(),
         title              :: b_string() | 'undefined',
         branch             :: b_string(),
         revision           :: b_string(),
         author             :: b_string(),
         start              :: time(),
         stop               :: time(),
         status             :: 'pending' | 'building' | 'success' | 'fail',
         exit               :: integer(),
         output             :: list(b_string()),
         tags               :: list(tag())
        }).

-record(id_seq,
        {whose              :: atom(),
         id = 10000         :: integer()
        }).

%%FIXME: PF: A temporary solution -> should by replace by alog or lager
-define(LOG(X,Y), (fun() -> io:fwrite("[~p] LOG [~p:~b]: ~s~n", [calendar:local_time(), ?MODULE, ?LINE, io_lib:fwrite(X, Y)]) end)()).
