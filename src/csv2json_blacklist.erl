-module(csv2json_blacklist).

-export([parse_file/1]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-include("address.hrl").

-record(blacklist_entry, {
    '_id'                          :: {string, string()},
    dst_addr                       :: #address{},
    src_addr                       :: #address{}
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([address, blacklist_entry]).

%% ===================================================================
%% API
%% ===================================================================

-spec parse_file(string()) -> {ok, [#blacklist_entry{}]}.
parse_file(Filename) ->
    {ok, Entries} = csv2json_lib:parse_file(Filename, fun parse_line/1),
    %io:format("~p~n", [Entries]),

    {ok, Entries}.

%% ===================================================================
%% Internal
%% ===================================================================

parse_line(Line) ->
    %io:format("~p~n", [Line]),
    {ID,          Line2}  = csv2json_lib:parse_uuid(Line),
    {Phone,       Line3}  = csv2json_lib:parse_string(Line2),
    {Originator,  Line4}  = csv2json_lib:parse_string(Line3),
    {_IncomingID, Line5}  = csv2json_lib:parse_string(Line4),
    {_CreatedBy,  Line6}  = csv2json_lib:parse_string(Line5),
    {_CreatedOn,  Line7}  = csv2json_lib:parse_string(Line6),
    {_ModifBy,    Line8}  = csv2json_lib:parse_string(Line7),
    {_ModifOn,       []}  = csv2json_lib:parse_string(Line8),
    #blacklist_entry{
        '_id' = ID,
        dst_addr = make_address(phone, Phone),
        src_addr = make_address(originator, Originator)
    }.

make_address(phone, {string, Digits} = Phone) when length(Digits) =/= 0 ->
    #address{
        addr = Phone,
        ton = {integer, 1},
        npi = {integer, 1}
    };
make_address(originator, {string, ""}) ->
    undefined;
make_address(originator, Originator) ->
    #address{
        addr = Originator,
        ton = {integer, 5},
        npi = {integer, 0}
    }.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_1_test() ->
    Line = "\"3311997b-ca3b-45c0-9a86-dbd1d71dd1f9\",\"1112233344\",\"AUDI\",\"\",\"86fe6fe0-ef22-4f0b-bbaf-1510262c8298\",\"25.08.2009 9:33:29\",\"\",\"\"",
    Actual = parse_line(Line),
    Expected = #blacklist_entry{
        '_id' = {string, "3311997b-ca3b-45c0-9a86-dbd1d71dd1f9"},
        dst_addr = #address{
            addr = {string, "1112233344"},
            ton  = {integer, 1},
            npi  = {integer, 1}
        },
        src_addr = #address{
            addr = {string, "AUDI"},
            ton  = {integer, 5},
            npi  = {integer, 0}
        }
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"_id\":\"3311997b-ca3b-45c0-9a86-dbd1d71dd1f9\",\"dst_addr\":{\"addr\":\"1112233344\",\"ton\":1,\"npi\":1},\"src_addr\":{\"addr\":\"AUDI\",\"ton\":5,\"npi\":0}}\n",
    ?assertEqual(ExpJson, Json).

parse_2_test() ->
    Line = "\"3311997b-ca3b-45c0-9a86-dbd1d71dd1f9\",\"1112233344\",\"\",\"\",\"86fe6fe0-ef22-4f0b-bbaf-1510262c8298\",\"25.08.2009 9:33:29\",\"\",\"\"",
    Actual = parse_line(Line),
    Expected = #blacklist_entry{
        '_id' = {string, "3311997b-ca3b-45c0-9a86-dbd1d71dd1f9"},
        dst_addr = #address{
            addr = {string, "1112233344"},
            ton  = {integer, 1},
            npi  = {integer, 1}
        },
        src_addr = undefined
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"_id\":\"3311997b-ca3b-45c0-9a86-dbd1d71dd1f9\",\"dst_addr\":{\"addr\":\"1112233344\",\"ton\":1,\"npi\":1},\"src_addr\":null}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
