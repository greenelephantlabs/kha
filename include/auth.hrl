-type accessor()  :: 'default' | {'user', username()}.
-type resource()  :: 'default' | {'project', project_id()}.
-type operation() :: 'read' | 'write'.

-record(session,
        {id        :: string(),
         username  :: username(),
         start     :: time(),
         expire    :: time()
        }).

-record(user,
        {name      :: username(),
         password  :: binary()
        }).

-record(acl,
        {key       :: tuple(accessor(), resource(), operation()),
         accessor  :: accessor(),
         resource  :: resource(),
         operation :: operation(),
         response  :: 'allow' | 'deny'
        }).
