-module(csv2json_lib).

-export([
    parse_file/2,
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

-spec parse_file(string(), fun()) -> [string()].
parse_file(Filename, Fun) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            [_ColumnNames | Lines] = string:tokens(binary_to_list(Bin), ?ROW_DELIM),
            ParsedLines = lists:map(Fun, Lines),
            {ok, ParsedLines};
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
    case parse_string(Str) of
        {{string, "1"}, Rest} ->
            {{boolean, true}, Rest};
        {{string, "0"}, Rest} ->
            {{boolean, false}, Rest}
    end.

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
    EncryptedB = crypto:des_cbc_encrypt(KeyB, IVecB, TextUPadded),
    Base64EncryptedB = base64:encode(EncryptedB),
    binary_to_list(Base64EncryptedB).

-spec des_decrypt(string(), string(), string()) -> string().
des_decrypt(Key, IVec, Cipher) ->
    KeyB = list_to_binary(Key),
    IVecB = list_to_binary(IVec),
    Base64EncryptedB = list_to_binary(Cipher),
    EncryptedB = base64:decode(Base64EncryptedB),
    TextUPadded = crypto:des_cbc_decrypt(KeyB, IVecB, EncryptedB),
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

split_field([ColWrapper | Rest], _ColDelim, ColWrapper) ->
    {Field, [ColWrapper | Rest2]} =
        lists:splitwith(fun(C) -> C =/= ColWrapper end, Rest),
    {[ColWrapper] ++ Field ++ [ColWrapper], Rest2};
split_field(Str, ColDelim, _ColWrapper) ->
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

split_field_1_test() ->
    Str = "\"1,2,3\",\"4\"",
    Actual = split_field(Str, $,, $"),
    Expected = {"\"1,2,3\"", ",\"4\""},
    ?assertEqual(Expected, Actual).

split_field_2_test() ->
    Str = "1,2,3,4",
    Actual = split_field(Str, $,, $"),
    Expected = {"1", ",2,3,4"},
    ?assertEqual(Expected, Actual).

%parse_{string, integer, float, boolean, uuid}_test

-record(outer, {
    filled_string,
    empty_string,
    filled_array,
    empty_array,
    integer,
    float,
    boolean_true,
    boolean_false,
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
        inner_record  = #inner{field = {string, "inner_field"}}
    },
    Json = record_to_json(Outer, ?MODULE),
    ExpJson = "{\"filled_string\":\"string\",\"empty_string\":\"\",\"filled_array\":[\"one\",\"two\",\"three\"],\"empty_array\":[],\"integer\":1,\"float\":1.0,\"boolean_true\":true,\"boolean_false\":false,\"inner_record\":{\"field\":\"inner_field\"}}\n",
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

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
