-module(csv2json).

-export([main/1]).

%% ===================================================================
%% API
%% ===================================================================

-spec main(list()) -> no_return().
main(["networks", NetworksFile, PrefixesFile]) ->
    {ok, Networks} = csv2json_networks:parse_files(NetworksFile, PrefixesFile),
    Jsons = [csv2json_lib:record_to_json(N, csv2json_networks) || N <- Networks],
    io:format("~s~n", [Jsons]),
    ok;
main(["network_maps", MapsFile, MappingFile]) ->
    {ok, Maps} = csv2json_network_maps:parse_files(MapsFile, MappingFile),
    Jsons = [csv2json_lib:record_to_json(M, csv2json_network_maps) || M <- Maps],
    io:format("~s~n", [Jsons]),
    ok;
main(["originators", OriginatorsFile]) ->
    {ok, Originators} = csv2json_originators:parse_file(OriginatorsFile),
    Jsons = [csv2json_lib:record_to_json(O, csv2json_originators) || O <- Originators],
    io:format("~s~n", [Jsons]),
    ok;
main(["users", UsersFile, Key, IVec]) ->
    Key2 = string_to_bytes(Key),
    IVec2 = string_to_bytes(IVec),
    {ok, Users} = csv2json_users:parse_file(UsersFile, Key2, IVec2),
    Jsons = [csv2json_lib:record_to_json(U, csv2json_users) || U <- Users],
    io:format("~s~n", [Jsons]),
    ok;
main(["customers", CustomersFile]) ->
    {ok, Customers} = csv2json_customers:parse_file(CustomersFile),
    Jsons = [csv2json_lib:record_to_json(C, csv2json_customers) || C <- Customers],
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
    io:format("Usage: ~s users <users file> <des key> <des ivec>~n", [BaseName]),

string_to_bytes(Str) ->
    [list_to_integer(X) || X <- string:tokens(Str, ",")].
