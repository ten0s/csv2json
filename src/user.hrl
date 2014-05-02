-ifndef(user_hrl).
-define(user_hrl, 1).

-record(user, {
    user_id          :: {string, string()},
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

-record(user_ref, {
    customer_id      :: {string, string()},
    user             :: #user{}
}).

-endif.
