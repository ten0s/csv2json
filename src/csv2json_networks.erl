-module(csv2json_networks).

-export([convert/2]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-record(prefix, {
    network_id                     :: {string, string()},
    prefix                         :: {string, string()}
}).

-record(network, {
    '_id'                          :: {string, string()},
    name                           :: {string, string()},
    country                        :: {string, string()},
    hex_code                       :: {string, string()},
    country_code                   :: {string, string()},
    number_len                     :: {integer, integer()},
    prefixes = {array, []}         :: {array, [{string, string()}]},
    gmt_diff                       :: {string, string()},
    dst                            :: {string, string()},
    provider_id                    :: {string, string()},
    is_home                        :: {boolean, boolean()},
    sms_points                     :: {float, float()},
    sms_mult_points = {float, 1.0} :: {float, float()}
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([prefix, network]).

%% ===================================================================
%% API
%% ===================================================================

-spec convert(string(), string()) -> [string()].
convert(NetworksFile, PrefixesFile) ->
    {ok, Prefixes} = parse_prefixes_file(PrefixesFile),
    %io:format("~p~n", [Prefixes]),
    {ok, Networks} = parse_networks_file(NetworksFile),
    %io:format("~p~n", [Networks]),

    Dict = build_prefixes_dict(Prefixes),
    Networks2 = set_prefixes_to_networks(Networks, Dict),
    %io:format("~p~n", [Networks2]),

    [csv2json_lib:record_to_json(N, ?MODULE) || N <- Networks2].

%% ===================================================================
%% Internal
%% ===================================================================

parse_networks_file(Filename) ->
    csv2json_lib:parse_file(Filename, fun(L) -> parse_network_line(L) end).

parse_prefixes_file(Filename) ->
    csv2json_lib:parse_file(Filename, fun(L) -> parse_prefix_line(L) end).

build_prefixes_dict(Prefixes) ->
    build_prefixes_dict(Prefixes, dict:new()).

build_prefixes_dict([P | Ps], Dict) ->
    NetworkId = P#prefix.network_id,
    Prefix = P#prefix.prefix,
    build_prefixes_dict(Ps, dict:append(NetworkId, Prefix, Dict));
build_prefixes_dict([], Dict) ->
    Dict.

set_prefixes_to_networks(Networks, PrefixesDict) ->
    set_prefixes_to_networks(Networks, PrefixesDict, []).

set_prefixes_to_networks([], _, Acc) ->
    lists:reverse(Acc);
set_prefixes_to_networks([N | Ns], Dict, Acc) ->
    NetworkId = N#network.'_id',
    N2 = case dict:find(NetworkId, Dict) of
            {ok, Prefixes} ->
                N#network{prefixes = {array, Prefixes}};
            error ->
                N
         end,
    set_prefixes_to_networks(Ns, Dict, [N2 | Acc]).

parse_network_line(Line) ->
    %io:format("~p~n", [Line]),
    {ID,           Line2}  = csv2json_lib:parse_uuid(Line),
    {Country,      Line3}  = csv2json_lib:parse_string(Line2),
    {Operator,     Line4}  = csv2json_lib:parse_string(Line3),
    {HexCode,      Line5}  = csv2json_lib:parse_string(Line4),
    {DialCode,     Line6}  = csv2json_lib:parse_string(Line5),
    {NumberLen,    Line7}  = csv2json_lib:parse_integer(Line6),
    {GMTDiff,      Line8}  = csv2json_lib:parse_string(Line7),
    {DST,          Line9}  = csv2json_lib:parse_string(Line8),
    {_OrigSupport, Line10} = csv2json_lib:parse_string(Line9),
    {SMSProvID,    Line11} = csv2json_lib:parse_uuid(Line10),
    {_MMSProvID,   Line12} = csv2json_lib:parse_uuid(Line11),
    {SMSPoints,    Line13} = csv2json_lib:parse_float(Line12),
    {_MMSPoints,   Line14} = csv2json_lib:parse_float(Line13),
    {_CreatedBy,   Line15} = csv2json_lib:parse_string(Line14),
    {_CreatedOn,   Line16} = csv2json_lib:parse_string(Line15),
    {_ModifBy,     Line17} = csv2json_lib:parse_string(Line16),
    {_ModifOn,     Line18} = csv2json_lib:parse_string(Line17),
    {IsHome,           []} = csv2json_lib:parse_boolean(Line18),
    #network{
        '_id' = ID,
        name = Operator,
        country = Country,
        hex_code = HexCode,
        country_code = DialCode,
        number_len = NumberLen,
        gmt_diff = GMTDiff,
        dst = DST,
        provider_id = SMSProvID,
        is_home = IsHome,
        sms_points = SMSPoints
    }.

parse_prefix_line(Line) ->
    %io:format("~p~n", [Line]),
    {NetworkId, Line2} = csv2json_lib:parse_uuid(Line),
    {Prefix,       []} = csv2json_lib:parse_string(Line2),
    #prefix{
        network_id = NetworkId,
        prefix = Prefix
    }.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_prefix_1_test() ->
    PrefixLine = "\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"312100\"",
    Prefix = parse_prefix_line(PrefixLine),
    ExpPrefix = #prefix{
        network_id = {string, "ccbcdea6-7c7a-4f01-8a08-005c30f89c3d"},
        prefix = {string, "312100"}
    },
    ?assertEqual(ExpPrefix, Prefix),

    Json = csv2json_lib:record_to_json(Prefix, ?MODULE),
    ExpJson = "{\"network_id\":\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"prefix\":\"312100\"}\n",
    ?assertEqual(ExpJson, Json).

parse_prefix_2_test() ->
    PrefixLine = "\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"\"",
    Prefix = parse_prefix_line(PrefixLine),
    ExpPrefix = #prefix{
        network_id = {string, "ccbcdea6-7c7a-4f01-8a08-005c30f89c3d"},
        prefix = {string, []}
    },
    ?assertEqual(ExpPrefix, Prefix),

    Json = csv2json_lib:record_to_json(Prefix, ?MODULE),
    ExpJson = "{\"network_id\":\"ccbcdea6-7c7a-4f01-8a08-005c30f89c3d\",\"prefix\":\"\"}\n",
    ?assertEqual(ExpJson, Json).

parse_network_1_test() ->
    NetworkLine = "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"Albania\",\"Mobile Comms\",\"\",\"355\",\"0\",\"+1\",\"7,5,3;7,5,10\",\"1\",\"65c4b123-2a51-49e1-84a8-b31dac878d8c\",\"\",\"5,0000\",\"5\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"26.05.2007 12:34:59\",\"86fe6fe0-ef22-4f0b-bbaf-1510262c8298\",\"13.01.2010 17:23:39\",\"0\"",
    Network = parse_network_line(NetworkLine),
    ExpNetwork = #network{
        '_id' = {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        name = {string, "Mobile Comms"},
        country = {string, "Albania"},
        hex_code = {string, ""},
        country_code = {string, "355"},
        number_len = {integer, 0},
        gmt_diff = {string, "+1"},
        dst = {string, "7,5,3;7,5,10"},
        provider_id = {string, "65c4b123-2a51-49e1-84a8-b31dac878d8c"},
        is_home = {boolean, false},
        sms_points = {float, 5.0}
    },
    ?assertEqual(ExpNetwork, Network),

    Json = csv2json_lib:record_to_json(Network, ?MODULE),
    ExpJson = "{\"_id\":\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"name\":\"Mobile Comms\",\"country\":\"Albania\",\"hex_code\":\"\",\"country_code\":\"355\",\"number_len\":0,\"prefixes\":[],\"gmt_diff\":\"+1\",\"dst\":\"7,5,3;7,5,10\",\"provider_id\":\"65c4b123-2a51-49e1-84a8-b31dac878d8c\",\"is_home\":false,\"sms_points\":5.0,\"sms_mult_points\":1.0}\n",
    ?assertEqual(ExpJson, Json).

parse_network_2_test() ->
    NetworkLine = "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"Albania\",\"Mobile Comms\",\"\",\"355\",\"0\",\"+1\",\"\",\"1\",\"\",\"\",\"5,0000\",\"5\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"26.05.2007 12:34:59\",\"86fe6fe0-ef22-4f0b-bbaf-1510262c8298\",\"13.01.2010 17:23:39\",\"0\"",
    Network = parse_network_line(NetworkLine),
    ExpNetwork = #network{
        '_id' = {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        name = {string, "Mobile Comms"},
        country = {string, "Albania"},
        hex_code = {string, ""},
        country_code = {string, "355"},
        number_len = {integer, 0},
        gmt_diff = {string, "+1"},
        dst = {string, ""},
        provider_id = {string, ""},
        is_home = {boolean, false},
        sms_points = {float, 5.0}
    },
    ?assertEqual(ExpNetwork, Network),

    Json = csv2json_lib:record_to_json(Network, ?MODULE),
    ExpJson = "{\"_id\":\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"name\":\"Mobile Comms\",\"country\":\"Albania\",\"hex_code\":\"\",\"country_code\":\"355\",\"number_len\":0,\"prefixes\":[],\"gmt_diff\":\"+1\",\"dst\":\"\",\"provider_id\":\"\",\"is_home\":false,\"sms_points\":5.0,\"sms_mult_points\":1.0}\n",
    ?assertEqual(ExpJson, Json).

build_prefixes_dict_test() ->
    Lines = [
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"663\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"664\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"669\""
    ],
    Prefixes = [parse_prefix_line(L) || L <- Lines],
    PrefixesDict = build_prefixes_dict(Prefixes),
    Actual = dict:fetch({string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"}, PrefixesDict),
    Expected = [{string, "663"}, {string, "664"}, {string, "669"}],
    ?assertEqual(Expected, Actual).

build_full_network_test() ->
    Lines = [
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"663\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"664\"",
        "\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"669\""
    ],
    Prefixes = [parse_prefix_line(L) || L <- Lines],
    PrefixesDict = build_prefixes_dict(Prefixes),

    Network = #network{
        '_id' = {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        name = {string, "Mobile Comms"},
        country = {string, "Albania"},
        hex_code = {string, ""},
        country_code = {string, "355"},
        number_len = {integer, 0},
        gmt_diff = {string, "+1"},
        dst = {string, "7,5,3;7,5,10"},
        provider_id = {string, "65c4b123-2a51-49e1-84a8-b31dac878d8c"},
        is_home = {boolean, false},
        sms_points = {float, 5.0}
    },

    [Network2] = set_prefixes_to_networks([Network], PrefixesDict),
    ExpectedPrefixes = {array, [{string, "663"}, {string, "664"}, {string, "669"}]},
    ActualPrefixes = Network2#network.prefixes,
    ?assertEqual(ExpectedPrefixes, ActualPrefixes),

    Json = csv2json_lib:record_to_json(Network2, ?MODULE),
    ExpJson = "{\"_id\":\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"name\":\"Mobile Comms\",\"country\":\"Albania\",\"hex_code\":\"\",\"country_code\":\"355\",\"number_len\":0,\"prefixes\":[\"663\",\"664\",\"669\"],\"gmt_diff\":\"+1\",\"dst\":\"7,5,3;7,5,10\",\"provider_id\":\"65c4b123-2a51-49e1-84a8-b31dac878d8c\",\"is_home\":false,\"sms_points\":5.0,\"sms_mult_points\":1.0}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
