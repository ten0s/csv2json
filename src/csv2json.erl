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
main(["users", UsersFile, Key, IVec]) ->
    ToIntList = fun(List) ->
        [list_to_integer(X) || X <- string:tokens(List, ",")]
    end,
    Key2 = ToIntList(Key),
    IVec2 = ToIntList(IVec),
    Jsons = csv2json_users:convert(UsersFile, Key2, IVec2),
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
    io:format("Usage: ~s originators <originators file>~n", [BaseName]),
    io:format("Usage: ~s users <users file> <des key> <des ivec>~n", [BaseName]).
