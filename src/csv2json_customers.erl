-module(csv2json_customers).

-export([parse_file/1]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-record(customer, {
    '_id'               :: {string, string()},
    customer_id         :: {string, string()},
    name                :: {string, string()},
    priority            :: {integer, integer()},
    rps                 :: {integer, integer()},
%    allowed_sources     :: [addr()],
%    default_source      :: addr() | undefined,
    network_map_id      :: {string, string()},
    default_provider_id :: {string, string()},
    receipts_allowed    :: {boolean, boolean()},
    no_retry            :: {boolean, boolean()},
    default_validity    :: {string, string()},
    max_validity        :: {integer, integer()},
%   users = []          :: [user()] | [],
    pay_type            :: {string, string()},
    credit              :: {float, float()},
    credit_limit        :: {float, float()},
    language            :: {string, string()},
    state               :: {string, string()}
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([customer]).

%% ===================================================================
%% API
%% ===================================================================

-spec parse_file(string()) -> [string()].
parse_file(CustomersFile) ->
    parse_customers_file(CustomersFile).


%% ===================================================================
%% Internal
%% ===================================================================

parse_customers_file(Filename) ->
    csv2json_lib:parse_file(Filename, fun parse_customer_line/1).

parse_customer_line(Line) ->
    %io:format("~p~n", [Line]),
    {ID,                     Line2} = csv2json_lib:parse_uuid(Line),
    {CustomerID,             Line3} = csv2json_lib:parse_uuid(Line2),
    {CustomerName,           Line4} = csv2json_lib:parse_string(Line3),
    {Credit,                 Line5} = csv2json_lib:parse_float(Line4),
    {CreditLimit,            Line6} = csv2json_lib:parse_float(Line5),
    {Priority,               Line7} = csv2json_lib:parse_integer(Line6),
    {_Edition,               Line8} = csv2json_lib:parse_string(Line7),
    {Blocked,                Line9} = csv2json_lib:parse_integer(Line8),
    {_CreatedBy,            Line10} = csv2json_lib:parse_string(Line9),
    {_CreatedOn,            Line11} = csv2json_lib:parse_string(Line10),
    {_ModifBy,              Line12} = csv2json_lib:parse_string(Line11),
    {_ModifOn,              Line13} = csv2json_lib:parse_string(Line12),
    {_WarnLimitNotifSent,   Line14} = csv2json_lib:parse_string(Line13),
    {MapID,                 Line15} = csv2json_lib:parse_uuid(Line14),
    {_FirstMessageRuleID,   Line16} = csv2json_lib:parse_string(Line15),
    {_InProgress,           Line17} = csv2json_lib:parse_string(Line16),
    {_LockServer,           Line18} = csv2json_lib:parse_string(Line17),
    {_LockTime,             Line19} = csv2json_lib:parse_string(Line18),
    {PayTypeID,             Line20} = csv2json_lib:parse_string(Line19),
    {_CreditMMS,            Line21} = csv2json_lib:parse_string(Line20),
    {_MtcType,              Line22} = csv2json_lib:parse_string(Line21),
    {_MtcMsisdn,            Line23} = csv2json_lib:parse_string(Line22),
    {_DistributorID,        Line24} = csv2json_lib:parse_string(Line23),
    {_MaxUsersCount,        Line25} = csv2json_lib:parse_string(Line24),
    {_DealerID,             Line26} = csv2json_lib:parse_string(Line25),
    {RouteProviderID,       Line27} = csv2json_lib:parse_uuid(Line26),
    {_MMSChargeFlag,        Line28} = csv2json_lib:parse_string(Line27),
    {_WebServiceAcceptIP,   Line29} = csv2json_lib:parse_string(Line28),
    {Language,              Line30} = csv2json_lib:parse_string(Line29),
    {_MonthLimit,           Line31} = csv2json_lib:parse_string(Line30),
    {_Plan,                 Line32} = csv2json_lib:parse_string(Line31),
    {_LastAccessed,         Line33} = csv2json_lib:parse_string(Line32),
    {_LastAccessedNotified, Line34} = csv2json_lib:parse_string(Line33),
    {_MtcMsisdnOld,         Line35} = csv2json_lib:parse_string(Line34),
    {_Custom2WayMSIDSN,     Line36} = csv2json_lib:parse_string(Line35),
    {_Custom2WayMsisdnMms , Line37} = csv2json_lib:parse_string(Line36),
    {_Custom2WayMsisdnOld,  Line38} = csv2json_lib:parse_string(Line37),
    {_Custom2WayMsisdnMmsOld,   []} = csv2json_lib:parse_string(Line38),
    #customer{
        '_id' = ID,
        customer_id = CustomerID,
        name = CustomerName,
        priority = Priority,
        rps = {integer, 10000},
        network_map_id = MapID,
        default_provider_id = RouteProviderID,
        receipts_allowed = {boolean, true},
        no_retry = {boolean, false},
        default_validity = {string, "000003000000000R"},
        max_validity = {integer, 259200},
        pay_type = process_pay_type(PayTypeID),
        credit = Credit,
        credit_limit = CreditLimit,
        language = Language,
        state = process_blocked(Blocked)
    }.

process_blocked({integer, Status}) ->
    Status2 =
        case Status of
            0 -> "active";
            1 -> "blocked";
            2 -> "deactivated";
            99 -> "deleted"
        end,
    {string, Status2}.

process_pay_type({string, PayTypeID}) ->
    PayType =
        case PayTypeID of
            "PT_POSTPAID" -> "postpaid";
            "PT_PREPAID"  -> "prepaid"
        end,
    {string, PayType}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_user_test() ->
    Line = "\"1f2a0fa3-f720-4fb5-8521-b7fbab088e7e\",\"1245\",\"STC\",\"-1406470,0000\",\"99999999999,0000\",\"5\",\"Standard\",\"0\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"08.03.2010 2:38:05\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"08.03.2010 11:36:15\",\"False\",\"93ae08b7-40ed-4fa7-b33e-02dbf59d44ee\",\"1\",\"0\",\"\",\"\",\"PT_POSTPAID\",\"0,0000\",\"CORPORATE\",\"\",\"\",\"10\",\"\",\"8b80645a-b108-4b54-9cd8-4e70e5d1ce4b\",\"\",\"\",\"en\",\"\",\"1\",\"30.01.2011 9:26:33\",\"False\",\"\",\"\",\"\",\"\",\"\"",

    Actual = parse_customer_line(Line),
    Expected = #customer{
        '_id'               = {string, "1f2a0fa3-f720-4fb5-8521-b7fbab088e7e"},
        customer_id         = {string, "1245"},
        name                = {string, "STC"},
        priority            = {integer, 5},
        rps                 = {integer, 10000},
        network_map_id      = {string, "93ae08b7-40ed-4fa7-b33e-02dbf59d44ee"},
        default_provider_id = {string, "8b80645a-b108-4b54-9cd8-4e70e5d1ce4b"},
        receipts_allowed    = {boolean, true},
        no_retry            = {boolean, false},
        default_validity    = {string, "000003000000000R"},
        max_validity        = {integer, 259200},
        pay_type            = {string, "postpaid"},
        credit              = {float, -1406470.0},
        credit_limit        = {float, 99999999999.0},
        language            = {string, "en"},
        state               = {string, "active"}
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"_id\":\"1f2a0fa3-f720-4fb5-8521-b7fbab088e7e\",\"customer_id\":\"1245\",\"name\":\"STC\",\"priority\":5,\"rps\":10000,\"network_map_id\":\"93ae08b7-40ed-4fa7-b33e-02dbf59d44ee\",\"default_provider_id\":\"8b80645a-b108-4b54-9cd8-4e70e5d1ce4b\",\"receipts_allowed\":true,\"no_retry\":false,\"default_validity\":\"000003000000000R\",\"max_validity\":259200,\"pay_type\":\"postpaid\",\"credit\":-1406470.0,\"credit_limit\":99999999999.0,\"language\":\"en\",\"state\":\"active\"}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
