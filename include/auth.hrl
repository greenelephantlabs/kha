-type accessor()  :: 'default' | {'user', email()}.
-type resource()  :: 'default' | {'project', project_id()}.
-type operation() :: 'read' | 'write'.

-record(session,
        {id        :: string(),
         email     :: email(),
         start     :: time(),
         expire    :: time()
        }).

-record(user,
        {email     :: email(),
         password  :: binary()
        }).

-record(acl,
        {key       :: tuple(accessor(), resource(), operation()),
         accessor  :: accessor(),
         resource  :: resource(),
         operation :: operation(),
         response  :: 'allow' | 'deny'
        }).
