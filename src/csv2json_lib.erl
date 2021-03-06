-module(csv2json_lib).

-export([
    parse_file/2,
    parse_file/3,
    parse_uuid/1,
    parse_integer/1,
    parse_float/1,
    parse_boolean/1,
    parse_string/1,
    record_to_json/2,
    des_encrypt/3,
    des_decrypt/3
]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-define(ROW_DELIM, "\r\n"). % "\r"
-define(COL_DELIM, $,).     % $|
-define(COL_WRAPPER, $").

%% ===================================================================
%% API
%% ===================================================================

-spec parse_file(string(), fun()) -> [term()].
parse_file(Filename, Fun) ->
    case csv2json_file:read_file(Filename) of
        {ok, Bin} ->
            [_ColumnNames | Lines] = string:tokens(binary_to_list(Bin), ?ROW_DELIM),
            ParsedLines = lists:map(Fun, Lines),
            {ok, ParsedLines};
        Error ->
            Error
    end.

-spec parse_file(string(), fun(), AccIn::term()) -> {[term()], AccOut::term()}.
parse_file(Filename, Fun2, AccIn) ->
    case csv2json_file:read_file(Filename) of
        {ok, Bin} ->
            [_ColumnNames | Lines] = string:tokens(binary_to_list(Bin), ?ROW_DELIM),
            FoldFun = fun(Line, {ParsedLines, Acc}) ->
                {ParsedLine, Acc2} = Fun2(Line, Acc),
                {[ParsedLine | ParsedLines], Acc2}
            end,
            {ParsedLines, AccOut} = lists:foldl(FoldFun, {[], AccIn}, Lines),
            {ok, {lists:reverse(ParsedLines), AccOut}};
        Error ->
            Error
    end.

-spec parse_uuid(string()) -> {string, string()}.
parse_uuid(Str) ->
    %io:format("uuid: ~p~n", [Str]),
    case parse_string(Str) of
        {{string, []}, Rest} ->
            {{string, []}, Rest};
        {{string, Value}, Rest} ->
            [UUID] = string:tokens(Value, "{}"),
            Uuid = string:to_lower(UUID),
            {{string, Uuid}, Rest}
    end.

-spec parse_integer(string()) -> {integer, integer()}.
parse_integer(Str) ->
    %io:format("integer: ~p~n", [Str]),
    {{string, Value}, Rest} = parse_string(Str),
    {{integer, list_to_integer(Value)}, Rest}.

-spec parse_float(string()) -> {float, float()}.
parse_float(Str) ->
    %io:format("float: ~p~n", [Str]),
    {{string, Value}, Rest} = parse_string(Str),
    {{float, convert_to_float(Value)}, Rest}.

-spec parse_boolean(string()) -> {boolean, boolean()}.
parse_boolean(Str) ->
    %io:format("boolean: ~p~n", [Str]),
    {{string, Value}, Rest} = parse_string(Str),
    {{boolean, convert_to_boolean(Value)}, Rest}.

-spec parse_string(string()) -> {string, string()}.
parse_string([]) ->
    %io:format("string: []~n"),
    {{string, []}, []};
parse_string(Str) ->
    %io:format("string: ~p~n", [Str]),
    case split_field(Str, ?COL_DELIM, ?COL_WRAPPER) of
        {Value, [?COL_DELIM | []]} ->
            {{string, strip(Value, ?COL_WRAPPER)}, []};
        {Value, [?COL_DELIM | Rest]} ->
            {{string, strip(Value, ?COL_WRAPPER)}, Rest};
        {Value, []} ->
            {{string, strip(Value, ?COL_WRAPPER)}, []}
    end.

-spec record_to_json(record(), module()) -> string().
record_to_json(Record, Module) ->
    Plist = record_info:record_to_proplist(Record, Module),
    proplist_to_json(Plist, Module).

-spec des_encrypt(string(), string(), string()) -> string().
des_encrypt(Key, IVec, Text) when is_list(Key), is_list(IVec), is_list(Text) ->
    KeyB = list_to_binary(Key),
    IVecB = list_to_binary(IVec),
    TextB = list_to_binary(Text),
    TextU = unicode:characters_to_binary(TextB, utf8, {utf16, little}),
    TextUPadded = append_padding('PKCS7', TextU),
    EncryptedB = crypto:block_encrypt(des_cbc, KeyB, IVecB, TextUPadded),
    Base64EncryptedB = base64:encode(EncryptedB),
    binary_to_list(Base64EncryptedB).

-spec des_decrypt(string(), string(), string()) -> string().
des_decrypt(Key, IVec, Cipher) ->
    KeyB = list_to_binary(Key),
    IVecB = list_to_binary(IVec),
    Base64EncryptedB = list_to_binary(Cipher),
    EncryptedB = base64:decode(Base64EncryptedB),
    TextUPadded = crypto:block_decrypt(des_cbc, KeyB, IVecB, EncryptedB),
    TextU = strip_padding('PKCS7', TextUPadded),
    TextB = unicode:characters_to_binary(TextU, {utf16, little}, utf8),
    binary_to_list(TextB).

%% ===================================================================
%% Internal
%% ===================================================================

%%
%% http://www.di-mgt.com.au/cryptopad.html#exampleecb
%% Method 1 - Pad with bytes all of the same value as the number of padding bytes
%%
append_padding('PKCS7', Bin) ->
    Size = erlang:size(Bin),
    PaddingSize = 8 - (Size rem 8),
    Padding = list_to_binary(lists:duplicate(PaddingSize, PaddingSize)),
    <<Bin/binary, Padding/binary>>.

%%
%% http://www.di-mgt.com.au/cryptopad.html#exampleecb
%% Method 1 - Pad with bytes all of the same value as the number of padding bytes
%%
strip_padding('PKCS7', Bin) ->
    Size = erlang:size(Bin),
    PaddingSize = binary:last(Bin),
    binary:part(Bin, 0, Size - PaddingSize).

split_field([ColWrapper | Rest], ColDelim, ColWrapper) ->
    case lists:splitwith(fun(C) -> C =/= ColWrapper end, Rest) of
        {Field, [ColWrapper, ColDelim | Rest2]} -> %% 2_test
            {[ColWrapper] ++ Field ++ [ColWrapper], [ColDelim | Rest2]};
        {Field, [ColWrapper | []]} ->              %% 2_1_test
            {[ColWrapper] ++ Field ++ [ColWrapper], []};
        {Field, [ColWrapper | Rest2]} ->           %% 3_*_test
            {Field2, Rest3} = split_field(Rest2, ColDelim, ColWrapper),
            {[C || C <- Field ++ Field2, C =/= ColWrapper], Rest3}
    end;
split_field(Str, ColDelim, _ColWrapper) -> %% 1_test
    lists:splitwith(fun(C) -> C =/= ColDelim end, Str).

strip(Str, Char) ->
    string:strip(string:strip(Str, both, Char)).

convert_to_float([]) ->
    0.0;
convert_to_float(List) ->
    try list_to_float(List)
    catch
        error:badarg ->
            try list_to_integer(List) of
                Int -> float(Int)
            catch
                error:badarg ->
                    case lists:suffix(".", List) of
                        true ->
                            convert_to_float(List ++ "0");
                        false ->
                            case lists:prefix(".", List) of
                                true ->
                                    convert_to_float("0" ++ List);
                                false ->
                                    erlang:error({bad_float, List})
                            end
                    end
            end
    end.

convert_to_boolean("1") ->
    true;
convert_to_boolean("True") ->
    true;
convert_to_boolean("0") ->
    false;
convert_to_boolean("False") ->
    false.

proplist_to_json(Plist, Module) ->
    proplist_to_json_no_cr(Plist, Module) ++ "\n".

proplist_to_json_no_cr(Plist, Module) ->
    lists:flatten(lists:reverse(["}" | proplist_to_json(Plist, Module, ["{"])])).

proplist_to_json([{Key, Value}], Module, Acc) ->
    KeyValue = format_key_value(Key, Module, Value),
    [KeyValue | Acc];
proplist_to_json([{Key, Value} | Plist], Module, Acc) ->
    KeyValue = format_key_value(Key, Module, Value),
    proplist_to_json(Plist, Module, [",", KeyValue | Acc]).

format_key_value(Key, Module, Value) ->
    [format_key(Key), ":", format_value(Value, Module)].

format_key(Key) ->
    io_lib:format("~p", [atom_to_list(Key)]).

format_value(undefined, _Module) ->
    "null";
format_value({string, []}, _Module) ->
    "\"\"";
format_value({string, Value}, _Module) ->
    io_lib:format("~1000000p", [Value]);
format_value({integer, Value}, _Module) ->
    integer_to_list(Value);
format_value({array, []}, _Module) ->
    "[]";
format_value({array, Array}, Module) ->
    Values = string:join([format_value(V, Module) || V <- Array], ","),
    "[" ++ Values ++ "]";
format_value({boolean, Value}, _Module) ->
    atom_to_list(Value);
format_value({float, Value}, _Module) ->
    io_lib:format("~p", [Value]);
format_value(Record, Module) ->
    Plist = record_info:record_to_proplist(Record, Module),
    proplist_to_json_no_cr(Plist, Module).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

split_field_0_test() ->
    Str = "\"1\",\"2\",\"3\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"\"1\"", ",\"2\",\"3\""},
    ?assertEqual(Expected, Actual).

split_field_1_test() ->
    Str = "1,2,3,4",
    Actual = split_field(Str, $,, $"),
    Expected = {"1", ",2,3,4"},
    ?assertEqual(Expected, Actual).

split_field_2_test() ->
    Str = "\"1,2,3\",\"4\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"\"1,2,3\"", ",\"4\""},
    ?assertEqual(Expected, Actual).

split_field_2_1_test() ->
    Str = "\"1,2,3\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"\"1,2,3\"", ""},
    ?assertEqual(Expected, Actual).

split_field_3_1_test() ->
    Str = "\"\"One\"Two\",\"1\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"OneTwo", ",\"1\""},
    ?assertEqual(Expected, Actual).

split_field_3_2_test() ->
    Str = "\"\"One\"Two\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"OneTwo", ""},
    ?assertEqual(Expected, Actual).

split_field_3_3_test() ->
    Str = "\"Zero\"One\"Two\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"ZeroOneTwo", ""},
    ?assertEqual(Expected, Actual).

split_field_4_test() ->
    Str = "\"0,0000\",\"1\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"\"0,0000\"", ",\"1\""},
    ?assertEqual(Expected, Actual).

convert_to_boolean_test() ->
    ?assert(convert_to_boolean("1")),
    ?assert(convert_to_boolean("True")),
    ?assertNot(convert_to_boolean("0")),
    ?assertNot(convert_to_boolean("False")).

-record(outer, {
    filled_string,
    empty_string,
    filled_array,
    empty_array,
    integer,
    float,
    boolean_true,
    boolean_false,
    undefined,
    inner_record
}).

-record(inner, {
    field
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([outer, inner]).

record_to_json_test() ->
    Outer = #outer{
        filled_string = {string, "string"},
        empty_string  = {string, ""},
        filled_array  = {array, [{string, "one"}, {string, "two"}, {string, "three"}]},
        empty_array   = {array, []},
        integer       = {integer, 1},
        float         = {float, 1.0},
        boolean_true  = {boolean, true},
        boolean_false = {boolean, false},
        undefined     = undefined,
        inner_record  = #inner{field = {string, "inner_field"}}
    },
    Json = record_to_json(Outer, ?MODULE),
    ExpJson = "{\"filled_string\":\"string\",\"empty_string\":\"\",\"filled_array\":[\"one\",\"two\",\"three\"],\"empty_array\":[],\"integer\":1,\"float\":1.0,\"boolean_true\":true,\"boolean_false\":false,\"undefined\":null,\"inner_record\":{\"field\":\"inner_field\"}}\n",
    ?assertEqual(ExpJson, Json).

des_encrypt_decrypt_test() ->
    K = [128,80,153,234,171,122,153,37],
    V = [144,122,103,79,15,148,253,85],
    T1 = "123",
    T2 = "1234",
    T3 = "12345",
    ?assertEqual(T1, des_decrypt(K, V, des_encrypt(K, V, T1))),
    ?assertEqual(T2, des_decrypt(K, V, des_encrypt(K, V, T2))),
    ?assertEqual(T3, des_decrypt(K, V, des_encrypt(K, V, T3))).

parse_file_test_() ->
    {foreach,
        fun() -> meck:new(csv2json_file) end,
        fun(_) -> meck:unload(csv2json_file) end,
        [{"parse_file Fun/1 test",
            fun() ->
                meck:expect(csv2json_file, read_file, fun(_Filename) ->
                    {ok, <<"#comment line\r\nf11,f12\r\nf21,f22\r\nf31,f32\r\n">>}
                end),
                ParseFun = fun(Line) ->
                    {Field1, Line2} = parse_string(Line),
                    {Field2,    []} = parse_string(Line2),
                    {Field1, Field2}
                end,
                {ok, ActualLines} = parse_file("dummy.csv", ParseFun),
                ExpectedLines = [
                    {{string, "f11"}, {string, "f12"}},
                    {{string, "f21"}, {string, "f22"}},
                    {{string, "f31"}, {string, "f32"}}
                ],
                ?assertEqual(ExpectedLines, ActualLines)
            end},
         {"parse_file Fun/2 test",
            fun() ->
                meck:expect(csv2json_file, read_file, fun(_Filename) ->
                    {ok, <<"#comment line\r\nf11,f12\r\nf21,f22\r\nf31,f32\r\n">>}
                end),
                ParseFun2 = fun(Line, Id) ->
                    {Field1, Line2} = parse_string(Line),
                    {Field2,    []} = parse_string(Line2),
                    {{{integer, Id}, Field1, Field2}, Id + 1}
                end,
                {ok, {ActualLines, _}} = parse_file("dummy.csv", ParseFun2, 1),
                ExpectedLines = [
                    {{integer, 1}, {string, "f11"}, {string, "f12"}},
                    {{integer, 2}, {string, "f21"}, {string, "f22"}},
                    {{integer, 3}, {string, "f31"}, {string, "f32"}}
                ],
                ?assertEqual(ExpectedLines, ActualLines)
            end}

        ]
    }.

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
