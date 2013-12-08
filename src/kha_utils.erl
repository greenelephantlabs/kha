%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @author Gleb Peregud <gleber.p@gmail.com>
%%% @copyright (C) 2012, Green Elephant Labs
%%% @doc
%%% Various utils functions
%%% @end
%%% Created : 30 Jul 2012 by Paul Peter Flis <pawel@flycode.pl>

-module(kha_utils).

-include("common.hrl").
-include("kha.hrl").
-include("auth.hrl").

-export([list_convert/2,
         convert/2,
         convert_opt/2,
         convert_safe/2,
         fmt/2,
         b2a/1,
         a2b/1,
         i2b/1,
         b2i/1,
         binarize/1,

         clean_filename/1,

         now_to_nice/1,
         now_sum/2,
         debug/1
        ]).

-export([sh_stream/4,
         sh/1,sh/2,sh/3,

         mktemp_dir/0, mktemp_dir/1, mktemp_dir/2]).

-export([record_field/1,
         record_to_list/1,

         build_to_plist/1,
         notification_to_plist/1,

         headers/0,
         get_app_path/0,
         get_app_path/1]).

record_field(acl) ->      record_info(fields, acl);
record_field(session) ->  record_info(fields, session);
record_field(user) ->     record_info(fields, user);
record_field(project) ->  record_info(fields, project);
record_field(build) ->    record_info(fields, build);
record_field(id_seq) ->   record_info(fields, id_seq);
record_field(revision) ->  record_info(fields, revision).

record_to_list(Record) ->
    [RecordName | RecordData] = tuple_to_list(Record),
    Fields = ?MODULE:record_field(RecordName),
    lists:zip(Fields, RecordData).

binarize(L) when is_list(L) ->
    [ {convert(K, bin), V} || {K, V} <- L ].

notification_to_plist(#notification{type = Type,
                                    params = Params}) ->
    [{<<"type">>, convert(Type, bin)},
     {<<"params">>, binarize(Params)}].

build_to_plist(#build{id          = Id,
                      project     = Project,
                      title       = Title,
                      branch      = Branch,
                      revision    = Revision,
                      author      = Author,
                      create_time = CreateTime,
                      start       = Start,
                      stop        = Stop,
                      status      = Status,
                      exit        = Exit,
                      output      = Output,
                      tags        = Tags,
                      dir         = Dir}) ->
    fltr(
      [{<<"id">>,       Id},
       {<<"project">>,  Project},
       {<<"title">>,    kha_utils:convert_safe(Title, bin)},
       {<<"branch">>,   kha_utils:convert_safe(Branch, bin)},
       {<<"revision">>, kha_utils:convert_opt(Revision, bin)},
       {<<"author">>,   kha_utils:convert_opt(Author, bin)},
       {<<"create">>,   kha_utils:now_to_nice(CreateTime)},
       {<<"start">>,    kha_utils:now_to_nice(Start)},
       {<<"stop">>,     kha_utils:now_to_nice(Stop)},
       {<<"status">>,   kha_utils:convert_safe(Status, bin)},
       {<<"exit">>,     Exit},
       {<<"output">>,   kha_utils:convert(lists:reverse(Output), bin)},
       {<<"tags">>,     kha_utils:list_convert(Tags, bin)},
       {<<"dir">>,      kha_utils:convert_opt(Dir, bin)}
      ]).

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

clean_filename(B) ->
    iolist_to_binary(string:to_lower(re:replace(B, "[^a-zA-Z0-9]+", "-", [global, {return, list}]))).

fltr(L) ->
    [ {K, V} || {K,V} <- L, V /= undefined ].

convert_opt(undefined, _Type) ->
    undefined;
convert_opt(Val, Type) ->
    convert(Val, Type).

convert_safe(undefined, Type) ->
    erlang:error({unable_convert, undefined, Type});
convert_safe(Val, Type) ->
    convert(Val, Type).

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

convert(Val, str)
  when is_list(Val) ->
    binary_to_list(iolist_to_binary(Val));

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


sh_stream(Cmd, Ref, Parent, Opts) ->
    Send = fun(Line, []) ->
                   Parent ! {Ref, line, Line},
                   []
           end,
    sh:sh(lists:flatten(convert(Cmd, str)), Opts ++ [{output_handler, Send}, return_on_error]).

sh(Cmd) ->
    sh(Cmd, []).
sh(Cmd, Opts) ->
    sh:sh(lists:flatten(convert(Cmd, str)), Opts ++ [{use_stdout, false}, return_on_error]).
sh(Cmd, Args, Opts) ->
    sh:sh(lists:flatten(convert(Cmd, str)), Args, Opts ++ [{use_stdout, false}, return_on_error]).


mktemp_dir() ->
    mktemp_dir("kha_build.").

mktemp_dir(Prefix) ->
    sh("mktemp -d \"~sXXXXX\"", [Prefix], []).

mktemp_dir(Prefix, Tmpdir) ->
    sh("mktemp -d \"~sXXXXX\" --tmpdir=\"~s\"", [Prefix, Tmpdir], []).

%% Interval is specified in milliseconds
-spec now_sum(erlang:timestamp(), integer()) -> erlang:timestamp().
now_sum({A0, B0, C0}, T0) ->
    T = T0 * 1000,
    M = 1000000,
    C = (C0 + T) rem M,
    CC = trunc((C0 + T) / M),
    B = (B0 + CC) rem M,
    CB = trunc((B0 + CC) / M),
    A = (A0 + CB) rem M,
    {A, B, C}.

debug(true) ->
    application:set_env(kha, debug, true);
debug(false) ->
    application:set_env(kha, debug, false).
