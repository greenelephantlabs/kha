-type project_id() :: integer().
-type build_id()   :: integer().
-type email()      :: binary().
-type time()       :: {integer(), integer(), integer()}.
-type tag()        :: string().
-type command()    :: string().
-type b_string()   :: binary().
-type key()        :: binary().
-type pvalue()     :: binary() | number() | [binary()] | [number()].
-type params()     :: [{key(), pvalue()}].
