%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% Mnesia database
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(db).

-include("kha.hrl").

-export([start/0,
         init/0
        ]).

-export([create_table/1,
         create_table/2,
         create_table/3,

         add_record/1,
         add_records/1,

         remove_record/2,
         remove_object/1,
         remove_objects/1,

         check_exist/2,

         get_record/2,
         get_record_by_index/3,
         select/2,
         get_all/1,
         get_match_object/1,
         get_last/2,

         transaction/1,

         get_current_id/1,
         get_next_id/1,
         get_next_id/2,
         update_current_id/2]).

%% API

start() ->
    ?LOG("Starting mnesia", []),
    ok = mnesia:start().

init() ->
    ?LOG("Init mnesia", []),
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} ->
            ?LOG("Database already exists", []),
            start();
        {error, Reason} ->
            ?LOG("Error: ~p", [Reason]);
        ok ->
            start(),
            init_sequences(),
            init_schema(),
            kha_project:create_fake()
    end.

%%% internal

init_sequences() ->
    add_record(#id_seq{whose = project}).

init_schema() ->
    create_table(project),
    create_table(build, ordered_set, [{index, [#build.revision]}]),
    create_table(id_seq).

create_table(Name) ->
    create_table(Name, set).
create_table(Name, Type) ->
    create_table(Name, Type, []).
create_table(Name, Type, Opts) ->
    ?LOG("Create '~p' table", [Name]),
    ok =  case mnesia:create_table(Name, [{disc_copies, [node()]},
                                          {type, Type},
                                          {attributes, kha_utils:record_field(Name)}] ++ Opts) of
              {atomic, ok} -> ok;
              {aborted, {already_exists, _}} -> ok
          end.

%% External function

remove_record(From, Id) ->
    case mnesia:transaction(fun() -> mnesia:delete(From, Id, write) end) of
        {error, _} ->
            {error, not_found};
        {atomic, ok} ->
            ok
    end.

remove_object(Object) ->
    case mnesia:transaction(fun() -> mnesia:delete_object(Object) end) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

remove_objects(Objects) ->
    case mnesia:transaction(fun() ->
                                    [remove_object(O) || O <- Objects]
                            end) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

add_record(R) ->
    add_records([R]).

add_records(RL) ->
    case mnesia:transaction(fun() ->
                                    [ mnesia:write(R) || R <- RL ]
                            end) of
        {atomic, _} ->
            ok;
        {aborted, Error} ->
            {error, Error}
    end.

get_record(From, Id) ->
    case mnesia:transaction(fun() -> mnesia:read(From, Id) end) of
        {aborted, Error} ->
            {error, Error};
        {atomic, []} ->
            {error, not_found};
        {atomic, [R]} ->
            {ok, R}
    end.

get_record_by_index(From, Val, Index) ->
    case mnesia:transaction(fun() -> mnesia:index_read(From, Val, Index) end) of
        {atomic, R} ->
            {ok, R};
        {aborted, Error} ->
            {error, Error}
    end.

select(From, MS) ->
    case mnesia:transaction(fun() -> mnesia:select(From, MS) end) of
        {atomic, R} ->
            {ok, R}
    end.

get_all(Table) ->
    case mnesia:transaction(fun() -> lists:flatten([ mnesia:read(Table, Id) || Id <- mnesia:all_keys(Table) ]) end) of
        {atomic, R} ->
            {ok, R};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_match_object(Object) ->
    case mnesia:transaction(fun() -> mnesia:match_object(Object) end) of
        {atomic, R} ->
            {ok, R};
        {aborted, Reason} ->
            {error, Reason}
    end.

check_exist(Where, Id) ->
    case get_record(Where, Id) of
        {error, not_found} -> false;
        {ok, _} -> true
    end.

get_current_id(Whose) ->
    mnesia:dirty_update_counter(id_seq, Whose, 0).

get_next_id(Whose) ->
    get_next_id(Whose, 1).
get_next_id(Whose, Step) ->
    mnesia:dirty_update_counter(id_seq, Whose, Step). %%GP: nie działa w tranzakcjach

update_current_id(Whose, Id) ->
    Current = get_current_id(Whose),
    Diff = Id-Current,
    case Diff of
        X when 0<X ->
            get_next_id(Whose, X),
            ok;
        X when 0>=X ->
            ok
    end.

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Res} ->
            {ok, Res};
        {aborted, {throw, X}} ->
            throw(X);
        {aborted, Error} ->
            {error, Error}
    end.

get_last(Table, N) ->
    try transaction(fun() ->
                            mnesia:foldr(fun(_, {A, C}) when C == N ->
                                                 throw({A,C});
                                            (R, {A, C}) ->
                                                 {[R|A], C+1}
                                         end, {[], 0}, Table)
                    end) of
        {ok, {A, _}} ->
            {ok, lists:reverse(A)};
        Error ->
            Error
    catch
        throw:{A, N} ->
            {ok, lists:reverse(A)}
    end.
