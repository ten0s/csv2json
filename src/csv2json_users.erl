-module(csv2json_users).

-export([convert/3]).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -compile(export_all).
-endif.

-record(user, {
    customer_id      :: {string, string()},
    id               :: {string, string()},
    password         :: {string, string()},
    connection_types :: {array, [{string, string()}]},
    state            :: {string, string()},
    mobile_phone     :: {integer, integer()},
    first_name       :: {string, string()},
    last_name        :: {string, string()},
    company          :: {string, string()},
    occupation       :: {string, string()},
    email            :: {string, string()},
    country          :: {string, string()},
    language         :: {string, string()}
}).

-include_lib("record_info/include/record_info.hrl").
-export_record_info([user]).

%% ===================================================================
%% API
%% ===================================================================

-spec convert(string(), [byte()], [byte()]) -> [string()].
convert(UsersFile, Key, IVec) ->
    {ok, Users} = parse_users_file(UsersFile, Key, IVec),
    %io:format("~p~n", [Users]),

    [csv2json_lib:record_to_json(U, ?MODULE) || U <- Users].

%% ===================================================================
%% Internal
%% ===================================================================

parse_users_file(Filename, Key, IVec) ->
    csv2json_lib:parse_file(Filename, fun(L) -> parse_user_line(L, Key, IVec) end).

parse_user_line(Line, Key, IVec) ->
    %io:format("~p~n", [Line]),
    {_ID,                  Line2} = csv2json_lib:parse_uuid(Line),
    {CustomerID,           Line3} = csv2json_lib:parse_uuid(Line2),
    {Username,             Line4} = csv2json_lib:parse_string(Line3),
    {Password,             Line5} = csv2json_lib:parse_string(Line4),
    {_AccessLevel,         Line6} = csv2json_lib:parse_string(Line5),
    {MobilePhone,          Line7} = csv2json_lib:parse_string(Line6),
    {FirstName,            Line8} = csv2json_lib:parse_string(Line7),
    {LastName,             Line9} = csv2json_lib:parse_string(Line8),
    {Company,             Line10} = csv2json_lib:parse_string(Line9),
    {Occupation,          Line11} = csv2json_lib:parse_string(Line10),
    {Email,               Line12} = csv2json_lib:parse_string(Line11),
    {Country,             Line13} = csv2json_lib:parse_string(Line12),
    {Blocked,             Line14} = csv2json_lib:parse_integer(Line13),
    {_CanReplicate,       Line15} = csv2json_lib:parse_string(Line14),
    {_CanMMS,             Line16} = csv2json_lib:parse_string(Line15),
    {_CreatedBy,          Line17} = csv2json_lib:parse_string(Line16),
    {_CreatedOn,          Line18} = csv2json_lib:parse_string(Line17),
    {_ModifBy,            Line19} = csv2json_lib:parse_string(Line18),
    {_ModifOn,            Line20} = csv2json_lib:parse_string(Line19),
    {_MMUID,              Line21} = csv2json_lib:parse_string(Line20),
    {_Inbox,              Line22} = csv2json_lib:parse_string(Line21),
    {_MonthLimit,         Line23} = csv2json_lib:parse_string(Line22),
    {Language,            Line24} = csv2json_lib:parse_string(Line23),
    {_ConsumeMonthLimit,  Line25} = csv2json_lib:parse_string(Line24),
    {_AllowAllEmailToSms, Line26} = csv2json_lib:parse_string(Line25),
    {_UserLevelAcceptanceIsUsed, []} = csv2json_lib:parse_string(Line26),
    Decrypt = fun(Text) -> decrypt(Key, IVec, Text) end,
    #user{
        customer_id = CustomerID,
        id = Decrypt(Username),
        password = Password,
        connection_types = {array, [{string, "mm"}, {string, "soap"}, {string, "oneapi"}]},
        state = process_blocked(Blocked),
        mobile_phone = Decrypt(MobilePhone),
        first_name = Decrypt(FirstName),
        last_name = Decrypt(LastName),
        company = Decrypt(Company),
        occupation = Decrypt(Occupation),
        email = Decrypt(Email),
        country = Decrypt(Country),
        language = Language
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

decrypt(Key, IVec, {string, Text}) ->
    %io:format("~p~n", [Text]),
    Text2 =
        case Text of
            "" -> "";
            "Service customer user" -> "Service customer user";
            "Local office" -> "Local office";
            Text -> csv2json_lib:des_decrypt(Key, IVec, Text)
        end,
    {string, Text2}.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

parse_user_test() ->
    Line = "\"e917de20-2af0-43c7-bead-adee0453177c\",\"0e47952f-22ed-48fe-a975-94a043a6da76\",\"oZjWRY2+/h5wBWtRtjtfHw==\",\"bba6aee5e30c314fb0a4fb916d32491z\",\"1\",\"P3zSKLoV/v1nXDp35hwEHme8X2dwIW9bvhi27OLxqkA=\",\"viRZIIeC73zzXkv1VbM4h5MUPb3le2os\",\"f2u8gA54yVB6DZpoai6SU27Hep9B8cQN\",\"Z2JsJxzSLqu8mV2QChl1Gw==\",\"YU/45ZnAYmg=\",\"oZjWRY2+/h5hApifzt3xpPfb0yvC7WJxcUoB2vHsDcA=\",\"YmOaS/yMN+nQWJBvFQirSg==\",\"0\",\"1\",\"1\",\"ae3b4951-92ae-41c5-83b0-6f33f1fd57b9\",\"26.05.2007 11:59:16\",\"097c6eea-fae5-452c-a6bf-90bb6100fae4\",\"18.02.2010 16:19:14\",\"723AEEC4012FD648\",\"1\",\"0\",\"en\",\"0\",\"1\",\"0\"",
    Key = [114,185,242,128,80,153,234,171],
    IVec = [122,153,37,54,178,41,143,55],
    Actual = parse_user_line(Line, Key, IVec),
    Expected = #user{
        customer_id      = {string, "0e47952f-22ed-48fe-a975-94a043a6da76"},
        id               = {string, "name"},
        password         = {string, "bba6aee5e30c314fb0a4fb916d32491z"},
        connection_types = {array, [{string, "mm"}, {string, "soap"}, {string, "oneapi"}]},
        state            = {string, "active"},
        mobile_phone     = {string, "111223334455"},
        first_name       = {string, "first name"},
        last_name        = {string, "last name"},
        company          = {string, "Company"},
        occupation       = {string, "IT"},
        email            = {string, "name@email.com"},
        country          = {string, "country"},
        language         = {string, "en"}
    },
    ?assertEqual(Expected, Actual),

    Json = csv2json_lib:record_to_json(Actual, ?MODULE),
    ExpJson = "{\"customer_id\":\"0e47952f-22ed-48fe-a975-94a043a6da76\",\"id\":\"name\",\"password\":\"bba6aee5e30c314fb0a4fb916d32491z\",\"connection_types\":[\"mm\",\"soap\",\"oneapi\"],\"state\":\"active\",\"mobile_phone\":\"111223334455\",\"first_name\":\"first name\",\"last_name\":\"last name\",\"company\":\"Company\",\"occupation\":\"IT\",\"email\":\"name@email.com\",\"country\":\"country\",\"language\":\"en\"}\n",
    ?assertEqual(ExpJson, Json).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
