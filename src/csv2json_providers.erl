-module(csv2json_providers).

-export([parse_file/1]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-record(provider, {
    '_id'                 :: {string, string()},
    name                  :: {string, string()},
    description           :: {string, string()},
    gateway_id            :: {string, string()},
    bulk_gateway_id       :: {string, string()},
    receipts_supported    :: {boolean, boolean()},
    sms_add_points = 0.0  :: {float, float()}
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([provider]).

%% ===================================================================
%% API
%% ===================================================================

-spec parse_file(string()) -> {ok, [#provider{}]}.
parse_file(Filename) ->
    {ok, Entries} = csv2json_lib:parse_file(Filename, fun parse_line/1),
    %io:format("~p~n", [Entries]),

    {ok, Entries}.

%% ===================================================================
%% Internal
%% ===================================================================

parse_line(Line) ->
    %io:format("~p~n", [Line]),
    {ID,                Line2}  = csv2json_lib:parse_uuid(Line),
    {ProviderName,      Line3}  = csv2json_lib:parse_string(Line2),
    {SMSTemplateID,     Line4}  = csv2json_lib:parse_string(Line3),
    {_MMSTemplateID,    Line5}  = csv2json_lib:parse_string(Line4),
    {SMSAddPoints,      Line6}  = csv2json_lib:parse_float(Line5),
    {_MMSAddPoints,     Line7}  = csv2json_lib:parse_string(Line6),
    {SMSExpectReports,  Line8}  = csv2json_lib:parse_boolean(Line7),
    {_MMSExpectReports, Line9}  = csv2json_lib:parse_string(Line8),
    {_CreatedBy,       Line10}  = csv2json_lib:parse_string(Line9),
    {_CreatedOn,       Line11}  = csv2json_lib:parse_string(Line10),
    {_ModifBy,         Line12}  = csv2json_lib:parse_string(Line11),
    {_ModifOn,         Line13}  = csv2json_lib:parse_string(Line12),
    {_Gateway,         Line14}  = csv2json_lib:parse_string(Line13),
    {Description,      Line15}  = csv2json_lib:parse_string(Line14),
    {_HexCodes,        Line16}  = csv2json_lib:parse_string(Line15),
    {_GatewayBlob,         []}  = csv2json_lib:parse_string(Line16),
    #provider{
        '_id' = ID,
        name = ProviderName,
        description = Description,
        gateway_id = SMSTemplateID,
        bulk_gateway_id = SMSTemplateID,
        receipts_supported = SMSExpectReports,
        sms_add_points = SMSAddPoints
    }.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_test() ->
    Line = "\"e5ecbb4f-8d1e-418d-a133-e8e35e9287b7\",\"Name\",\"5bb0945f-f25f-407e-b6cc-5c970df3ddf2\",\"5bb0945f-f25f-407e-b6cc-5c970df3ddf2\",\"0,0001\",\"0,0000\",\"True\",\"False\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"26.05.2007 11:34:54\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"15.03.2012 12:33:49\",\"1\",\"Description\",\"64-0x40,163-0xA3\",\"1\"",
    Actual = parse_line(Line),
    Expected = #provider{
        '_id' = {string, "e5ecbb4f-8d1e-418d-a133-e8e35e9287b7"},
        name = {string, "Name"},
        description = {string, "Description"},
        gateway_id = {string, "5bb0945f-f25f-407e-b6cc-5c970df3ddf2"},
        bulk_gateway_id = {string, "5bb0945f-f25f-407e-b6cc-5c970df3ddf2"},
        receipts_supported = {boolean, true},
        sms_add_points = {float, 0.0001}
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"_id\":\"e5ecbb4f-8d1e-418d-a133-e8e35e9287b7\",\"name\":\"Name\",\"description\":\"Description\",\"gateway_id\":\"5bb0945f-f25f-407e-b6cc-5c970df3ddf2\",\"bulk_gateway_id\":\"5bb0945f-f25f-407e-b6cc-5c970df3ddf2\",\"receipts_supported\":true,\"sms_add_points\":0.0001}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
