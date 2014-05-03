-module(csv2json_originators).

-export([parse_file/1]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-include("originator.hrl").
-include_lib("record_info/include/record_info.hrl").
-export_record_info([address, originator, originator_ref]).

%% ===================================================================
%% API
%% ===================================================================

-spec parse_file(string()) -> {ok, [#originator{}]}.
parse_file(Filename) ->
    {ok, {Lines, _}} = csv2json_lib:parse_file(Filename, fun parse_line/2, dict:new()),
    {ok, Lines}.

%% ===================================================================
%% Internal
%% ===================================================================

parse_line(Line, Dict) ->
    %io:format("~p~n", [Line]),
    {_ID,           Line2} = csv2json_lib:parse_uuid(Line),
    {CustomerID,    Line3} = csv2json_lib:parse_uuid(Line2),
    {Originator,    Line4} = csv2json_lib:parse_string(Line3),
    {Description,   Line5} = csv2json_lib:parse_string(Line4),
    {Status,        Line6} = csv2json_lib:parse_integer(Line5),
    {_CreatedBy,    Line7} = csv2json_lib:parse_string(Line6),
    {_CreatedOn,    Line8} = csv2json_lib:parse_string(Line7),
    {_ModifBy,      Line9} = csv2json_lib:parse_string(Line8),
    {_ModifOn,     Line10} = csv2json_lib:parse_string(Line9),
    {IsDefault,    Line11} = csv2json_lib:parse_boolean(Line10),
    {_Restricted,  Line12} = csv2json_lib:parse_string(Line11),
    {_BypassBlacklist, []} = csv2json_lib:parse_string(Line12),
    Address = process_originator(Originator),
    Status2 = process_status(Status),
    Dict2 = dict:update_counter(CustomerID, 1, Dict),
    Id = dict:fetch(CustomerID, Dict2),
    OriginatorRef = #originator_ref{
        customer_id = CustomerID,
        originator = #originator{
            id          = {integer, Id},
            address     = Address,
            description = Description,
            state       = Status2,
            is_default  = IsDefault
        }
    },
    {OriginatorRef, Dict2}.

process_originator({string, Originator}) ->
    case all_digits(Originator) of
        true ->
            #address{addr = {string, Originator},
                     ton  = {integer, 1},
                     npi  = {integer, 1}};
        false ->
            #address{addr = {string, Originator},
                     ton  = {integer, 5},
                     npi  = {integer, 0}}
    end.

process_status({integer, Status}) ->
    Status2 =
        case Status of
            0 -> "pending";
            1 -> "approved";
            2 -> "rejected"
        end,
    {string, Status2}.

all_digits(String) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, String).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

all_digits_test() ->
    ?assert(all_digits("0987654321")),
    ?assertNot(all_digits("Hello")).

parse_line_test() ->
    Line = "\"b9a5c103-cb86-4770-9678-68f6538ab2cb\",\"f1aa2d75-b597-4ab1-8cca-094bc121da7b\",\"Facebook\",\"Facebook description\",\"1\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"24.10.2007 10:56:56\",\"\",\"\",\"0\",\"0\",\"1\"",
    {Actual, _} = parse_line(Line, dict:new()),
    Expected = #originator_ref{
        customer_id = {string, "f1aa2d75-b597-4ab1-8cca-094bc121da7b"},
        originator = #originator{
            id = {integer, 1},
            address = #address{
                addr = {string, "Facebook"},
                ton  = {integer, 5},
                npi  = {integer, 0}
            },
            description = {string, "Facebook description"},
            state       = {string, "approved"},
            is_default  = {boolean, false}
        }
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"customer_id\":\"f1aa2d75-b597-4ab1-8cca-094bc121da7b\",\"originator\":{\"id\":1,\"address\":{\"addr\":\"Facebook\",\"ton\":5,\"npi\":0},\"description\":\"Facebook description\",\"state\":\"approved\",\"is_default\":false}}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
