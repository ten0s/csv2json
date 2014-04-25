-module(csv2json_network_maps).

-export([convert/2]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-record(map2network, {
    map_id                    :: {string, string()},
    network_id                :: {string, string()}
}).

-record(network_map, {
    '_id'                     :: {string, string()},
    name                      :: {string, string()},
    network_ids = {array, []} :: {array, [{string, string()}]}
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([map2network, network_map]).

%% ===================================================================
%% API
%% ===================================================================

-spec convert(string(), string()) -> [string()].
convert(MapsFile, MappingsFile) ->
    {ok, Mappings} = parse_mappings_file(MappingsFile),
    %io:format("~p~n", [Mappings]),
    {ok, Maps} = parse_maps_file(MapsFile),
    %io:format("~p~n", [Maps]),

    Dict = build_networks_dict(Mappings),
    Maps2 = set_networks_to_maps(Maps, Dict),
    %io:format("~p~n", [Maps2]),

    [csv2json_lib:record_to_json(M, ?MODULE) || M <- Maps2].

%% ===================================================================
%% Internal
%% ===================================================================

parse_mappings_file(Filename) ->
    csv2json_lib:parse_file(Filename, fun(L) -> parse_mapping_line(L) end).

parse_maps_file(Filename) ->
    csv2json_lib:parse_file(Filename, fun(L) -> parse_map_line(L) end).

build_networks_dict(Mappings) ->
    build_networks_dict(Mappings, dict:new()).

build_networks_dict([M | Ms], Dict) ->
    MapId = M#map2network.map_id,
    NetworkId = M#map2network.network_id,
    build_networks_dict(Ms, dict:append(MapId, NetworkId, Dict));
build_networks_dict([], Dict) ->
    Dict.

set_networks_to_maps(Networks, PrefixesDict) ->
    set_networks_to_maps(Networks, PrefixesDict, []).

set_networks_to_maps([], _, Acc) ->
    lists:reverse(Acc);
set_networks_to_maps([M | Ms], Dict, Acc) ->
    MapId = M#network_map.'_id',
    M2 = case dict:find(MapId, Dict) of
            {ok, Networks} ->
                M#network_map{network_ids = {array, Networks}};
            error ->
                M
         end,
    set_networks_to_maps(Ms, Dict, [M2 | Acc]).

parse_map_line(Line) ->
    %io:format("~p~n", [Line]),
    {ID,   Line2}  = csv2json_lib:parse_uuid(Line),
    {Name, Line3}  = csv2json_lib:parse_string(Line2),
    {_Type,   []}  = csv2json_lib:parse_string(Line3),
    #network_map{
        '_id' = ID,
        name = Name
    }.

parse_mapping_line(Line) ->
    %io:format("~p~n", [Line]),
    {MapId,  Line2} = csv2json_lib:parse_uuid(Line),
    {NetworkId, []} = csv2json_lib:parse_uuid(Line2),
    #map2network{
        map_id = MapId,
        network_id = NetworkId
    }.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_mapping_test() ->
    Line = "\"59ca05dd-2818-46ce-8055-ae7d51df7486\",\"594edd56-089e-4198-b108-4c3d0a3910ec\"",
    Actual = parse_mapping_line(Line),
    Expected = #map2network{
        map_id = {string, "59ca05dd-2818-46ce-8055-ae7d51df7486"},
        network_id = {string, "594edd56-089e-4198-b108-4c3d0a3910ec"}
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"map_id\":\"59ca05dd-2818-46ce-8055-ae7d51df7486\",\"network_id\":\"594edd56-089e-4198-b108-4c3d0a3910ec\"}\n",
    ?assertEqual(ExpJson, Json).

parse_map_test() ->
    Line = "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"g#1 + g#2 + ksa\",\"\"",
    Actual = parse_map_line(Line),
    Expected = #network_map{
        '_id' = {string, "20b8333b-cc64-4f29-9bad-d1deaf314103"},
        name = {string, "g#1 + g#2 + ksa"}
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"_id\":\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"name\":\"g#1 + g#2 + ksa\",\"network_ids\":[]}\n",
    ?assertEqual(ExpJson, Json).

build_networks_dict_test() ->
    Lines = [
        "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\"",
        "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"4db28e38-9920-4930-8807-15adda02bdd2\"",
        "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"46598204-33ed-440d-a6a0-c55bf7eb9e3d\""
    ],
    Mappings = [parse_mapping_line(L) || L <- Lines],
    Dict = build_networks_dict(Mappings),
    Actual = dict:fetch({string, "20b8333b-cc64-4f29-9bad-d1deaf314103"}, Dict),
    Expected = [
        {string, "fca67c04-0b8e-4ac4-a542-6dcaf8f8da17"},
        {string, "4db28e38-9920-4930-8807-15adda02bdd2"},
        {string, "46598204-33ed-440d-a6a0-c55bf7eb9e3d"}
    ],
    ?assertEqual(Expected, Actual).

build_full_map_test() ->
    Lines = [
        "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\"",
        "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"4db28e38-9920-4930-8807-15adda02bdd2\"",
        "\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"46598204-33ed-440d-a6a0-c55bf7eb9e3d\""
    ],
    Mappings = [parse_mapping_line(L) || L <- Lines],
    Dict = build_networks_dict(Mappings),
    Map = #network_map{
        '_id' = {string, "20b8333b-cc64-4f29-9bad-d1deaf314103"},
        name = {string, "Name"}
    },
    [Map2] = set_networks_to_maps([Map], Dict),
    Json = csv2json_lib:record_to_json(Map2, ?MODULE),
    ExpJson = "{\"_id\":\"20b8333b-cc64-4f29-9bad-d1deaf314103\",\"name\":\"Name\",\"network_ids\":[\"fca67c04-0b8e-4ac4-a542-6dcaf8f8da17\",\"4db28e38-9920-4930-8807-15adda02bdd2\",\"46598204-33ed-440d-a6a0-c55bf7eb9e3d\"]}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
