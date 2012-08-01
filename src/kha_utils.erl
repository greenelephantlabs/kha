%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @author Gleb Peregud <gleber.p@gmail.com>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% Various utils functions
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_utils).

-include("kha.hrl").

-export([to_int/1,
         list_convert/2,
         convert/2,
         fmt/2,
         b2a/1,
         a2b/1,
         i2b/1,
         b2i/1,

         now_to_nice/1]).

-export([record_field/1,
         project_to_term/1,
         build_to_term/1,
         headers/0,
         get_app_path/0,
         get_app_path/1]).

-export([sh/1,
         sh/2]).

record_field(project) ->
    record_info(fields, project);
record_field(build) ->
    record_info(fields, build);
record_field(id_seq) ->
    record_info(fields, id_seq).

project_to_term(#project{id            = Id,
                         name          = Name,
                         local         = Local,
                         remote        = Remote,
                         build         = _Build,
                         notifications = _Notification}) ->
    [{<<"id">>, Id},
     {<<"name">>, kha_utils:convert(Name, bin)},
     {<<"local">>, kha_utils:convert(Local, bin)},
     {<<"remote">>, kha_utils:convert(Remote, bin)}
    ].

build_to_term(#build{id       = Id,
                     project  = Project,
                     title    = Title,
                     branch   = Branch,
                     revision = Revision,
                     author   = Author,
                     start    = Start,
                     stop     = Stop,
                     status   = Status,
                     exit     = Exit,
                     output   = Output,
                     tags     = Tags}) ->
    [{<<"id">>, Id},
     {<<"project">>, Project},
     {<<"title">>, kha_utils:convert(Title, bin)},
     {<<"branch">>, kha_utils:convert(Branch, bin)},
     {<<"revision">>, kha_utils:convert(Revision, bin)},
     {<<"author">>, kha_utils:convert(Author, bin)},
     {<<"start">>, kha_utils:now_to_nice(Start)},
     {<<"stop">>, kha_utils:now_to_nice(Stop)},
     {<<"status">>, kha_utils:convert(Status, bin)},
     {<<"exit">>, Exit},
     {<<"output">>, kha_utils:convert(lists:reverse(Output), bin)},
     {<<"tags">>, kha_utils:list_convert(Tags, bin)}
    ].

to_int(X) when is_binary(X) -> to_int(binary_to_list(X));
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_integer(X) -> X;
to_int(X) when X==undefined -> undefined;
to_int(X) when is_atom(X) -> to_int(atom_to_list(X)).

fmt(S, A) ->
    convert(io_lib:format(S, A), bin).

b2a(B) when is_binary(B) ->
    binary_to_atom(B, latin1).

a2b(A) when is_atom(A) ->
    atom_to_binary(A, latin1).

i2b(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I)).
b2i(I) when is_binary(I) ->
    list_to_integer(binary_to_list(I)).

list_convert(L, To) ->
    [ convert(Val, To) || Val <- L ].

now_to_nice(undefined) ->
    <<"undefined">>;
now_to_nice(Now) ->
    {{Y,M,D}, {H,Min, S}} = calendar:now_to_local_time(Now),
    Out = io_lib:fwrite("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y,M,D, H,Min,S]),
    convert(lists:flatten(Out), bin).

convert(Val, int)
  when is_list(Val) ->
    list_to_integer(Val);

convert(Val, int)
  when is_binary(Val) ->
    list_to_integer(binary_to_list(Val));

convert(Val, atom)
  when is_list(Val) ->
    list_to_atom(Val);

convert(Val, atom)
  when is_binary(Val) ->
    b2a(Val);

convert(Val, str)
  when is_integer(Val) ->
    integer_to_list(Val);

convert(Val, str)
  when is_float(Val) ->
    float_to_list(Val);

convert(Val, str)
  when is_atom(Val) ->
    atom_to_list(Val);

convert(Val, str)
  when is_binary(Val) ->
    binary_to_list(Val);

convert(Val, bin)
  when is_list(Val) ->
    iolist_to_binary(Val);

convert(Val, bin)
  when is_atom(Val) ->
    a2b(Val);

convert(Val, bin)
  when is_binary(Val) ->
    Val;

convert(undefined, bool) ->
    false;

convert(false, bool) ->
    false;

convert("", bool) ->
    false;

convert("0", bool) ->
    false;

convert("false", bool) ->
    false;

convert(<<"">>, bool) ->
    false;

convert(<<"0">>, bool) ->
    false;

convert(<<"false">>, bool) ->
    false;

convert(_, bool) ->
    true;

convert(Val, _) ->
    Val.

headers() ->
    [{<<"Content-Type">>, <<"application/json">>},
     {<<"Cache-Control">>, <<"max-age=0, private">>},
     {<<"Date">>, <<"Sun, 03 Jun 2012 16:31:11 GMT">>},
     {<<"Expires">>, <<"Sun, 03 Jun 2012 16:31:10 GMT">>}].

get_app_path() ->
    get_app_path("kha").
get_app_path(App) ->
    AppFile = App++".app",
    FilePath = code:where_is_file(AppFile),
    FilePath2 = filename:dirname(filename:absname(FilePath)),
    filename:join([FilePath2, "../"]).

%% @doc Exec given command.
%% @throws {exec_error, {Command, ErrCode, Output}}.
-spec sh(list(), list()) -> list().
sh(Command, Opts) ->
    ?LOG("CMD: ~s", [Command]),
    Command2 = lists:flatten(io_lib:fwrite("~s", [Command])),
    Port = open_port({spawn, Command2}, Opts ++ [
                                                 exit_status, {line, 255}, stderr_to_stdout
                                                ]),

    case sh_receive_loop(Port, []) of
        {ok, Data} -> Data;
        {error, {ErrCode, Output}} ->
            throw({exec_error, {Command, ErrCode, Output}})
    end.

sh(Command) ->
    sh(Command, []).

sh_receive_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} -> sh_receive_loop(Port, [Line ++ "\n"|Acc]);
        {Port, {data, {noeol, Line}}} ->
            sh_receive_loop(Port, [Line|Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, E}} ->
            {error, {E, lists:flatten(lists:reverse(Acc))}}
    end.

