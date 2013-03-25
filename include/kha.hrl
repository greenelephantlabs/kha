%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @author Gleb Peregud <gleber.p@gmail.com>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% RECORD DEFINITIONS
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-record(notification,
        {type               :: atom(), %% kha_notification_[type].erl
         params             :: params()
        }).

-record(project,
        {id                 :: project_id(),
         server             :: pid(),
         name               :: b_string(),
         remote             :: b_string(),
         build = []         :: list(command()),
         params = []        :: params(),
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
         tags               :: list(tag()),
         dir                :: filename:filename() %% git clone remote local
        }).

-record(id_seq,
        {whose              :: atom(),
         id = 10000         :: integer()
        }).

-record(revision, %%GP: this name does not say much. What's the purpose of this table?
        {key                :: {b_string(), b_string}, %% {remote, branch}
         rev                :: b_string()
        }
       ).
%%FIXME: PF: A temporary solution -> should by replace by alog or lager
-define(LOG(X,Y), case application:get_env(debug) of
                      undefined   -> ok;
                      {ok, false} -> ok;
                      {ok, _} -> io:fwrite("[~p] log [~p:~b]: "++ X ++ "~n",
                                               [calendar:local_time(), ?MODULE, ?LINE] ++ Y)
                  end).
