-module(csv2json).

-export([main/1]).

%% ===================================================================
%% API
%% ===================================================================

-spec main(list()) -> no_return().
main(["networks", NetworksFile, PrefixesFile]) ->
    Jsons = csv2json_networks:convert(NetworksFile, PrefixesFile),
    io:format("~s~n", [Jsons]),
    ok;
main(["network_maps", MapsFile, MappingFile]) ->
    Jsons = csv2json_network_maps:convert(MapsFile, MappingFile),
    io:format("~s~n", [Jsons]),
    ok;
main(["originators", OriginatorsFile]) ->
    Jsons = csv2json_originators:convert(OriginatorsFile),
    io:format("~s~n", [Jsons]),
    ok;
main(_) ->
    usage().

%% ===================================================================
%% Internal
%% ===================================================================

usage() ->
    ScriptName = escript:script_name(),
    BaseName = filename:basename(ScriptName),
    io:format("Usage: ~s networks <networks file> <prefixes file>~n", [BaseName]),
    io:format("Usage: ~s network_maps <maps file> <maps to networks file>~n", [BaseName]),
    io:format("Usage: ~s originators <originators file>~n", [BaseName]).
