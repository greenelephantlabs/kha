-module(acl).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/auth.hrl").

-export([read/3,

         check/1, check/3, check_user/3, check_user/4,

         define/4]).

-export([web/1, web_check/3]).

-define(DEFAULT, allow).

lst(L) when is_list(L) ->
    lists:flatten(L);
lst(X) ->
    lists:flatten([X]).

read(Accessor, Resource, Operation) ->
    case db:get_record(acl, {Accessor, Resource, Operation}) of
        {ok, #acl{response = Resp}} ->
            Resp;
        _ ->
            ?DEFAULT
    end.

check(Accessor, Resource, Operation) ->
    check([{A, R, O} || A <- lst(Accessor) ++ [default],
                        R <- lst(Resource) ++ [default],
                        O <- lst(Operation) ]).

check(Ops) when is_list(Ops) ->
    case db:get_many(acl, Ops) of
        {ok, []} ->
            ?DEFAULT;
        {ok, [#acl{response = Resp}|_]} ->
            Resp
    end.

check_user(User, Thing, Operation) ->
    check({user, User}, Thing, Operation).

check_user(User, Thing, Operation, Fun) ->
    case check_user(User, Thing, Operation) of
        ok ->
            Fun();
        {error, Error} ->
            {error, Error}
    end.

accessor(default = X) -> X;
accessor(logged = X) -> X;
accessor({user, Email} = X) when is_binary(Email) -> X.
resource(default = X) -> X;
resource({project, ProjectId} = X) when is_integer(ProjectId) -> X.
operation(read = X) -> X;
operation(write = X) -> X.
response(allow = X) -> X;
response(deny = X) -> X.

define(Accessor, Resource, Operation, Response) ->
    db:transaction(fun() ->
                           [ define0(A, R, O, Response) || A <- lst(Accessor),
                                                           R <- lst(Resource),
                                                           O <- lst(Operation) ]
                   end).

define0(Accessor, Resource, Operation, Response) ->
    Key = {accessor(Accessor), resource(Resource), operation(Operation)},
    Acl =
        #acl{key = Key,
             accessor = Accessor,
             resource = Resource,
             operation = Operation,
             response = response(Response)},
    db:add_record(Acl).

web_check(Req, Resource, Operation) ->
    case acl:check(session:as_acl(), Resource, Operation) of
        allow ->
            ok;
        deny ->
            throw({"", 401, Req})
    end.

web(Fun) ->
    try
        Fun()
    catch
        throw:{R, C, Rq} ->
            {R, C, Rq}
    end.
