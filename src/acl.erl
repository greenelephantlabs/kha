-module(acl).

-include_lib("kha/include/common.hrl").
-include_lib("kha/include/auth.hrl").

-export([check_user/3, check_user/4, define/4]).

check(Accessor, Resource, Operation) ->
    check([{Accessor, Resource, Operation}]).

check(Ops) when is_list(Ops) ->
    case db:get_many(acl, Ops) of
        [] ->
            allow;
        [#acl{response = Resp}|_] ->
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

accessor({user, Username} = X) when is_binary(Username) -> X.
resource({project, ProjectId} = X) when is_integer(ProjectId) -> X.
operation(read = X) -> X;
operation(write = X) -> X.
response(allow = X) -> X;
response(deny = X) -> X.

define(Accessor, Resource, Operation, Response) ->
    Key = {accessor(Accessor), resource(Resource), operation(Operation)},
    Acl =
        #acl{key = Key,
             accessor = Accessor,
             resource = Resource,
             operation = Operation,
             response = response(Response)},
    db:add_record(Acl).
