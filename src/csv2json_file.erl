-module(csv2json_file).

-export([read_file/1]).

%% ===================================================================
%% API
%% ===================================================================

-spec read_file(Filename) -> {ok, Binary} | {error, Reason} when
    Filename :: file:name_all(),
    Binary   :: binary(),
    Reason   :: file:posix() | badarg | terminated | system_limit.
read_file(Filename) ->
    %% The only reason this call exists because meck currently
    %% doesn't support mocking the `file' module.
    file:read_file(Filename).
