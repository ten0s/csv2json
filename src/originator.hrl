-ifndef(originator_hrl).
-define(originator_hrl, 1).

-record(address, {
    addr                      :: {string, string()},
    ton                       :: {integer, integer()},
    npi                       :: {integer, integer()}
}).

-record(originator, {
    customer_id               :: {string, string()},
    address                   :: #address{},
    description               :: {string, string()},
    status                    :: {string, string()},
    is_default                :: {boolean, boolean()}
}).

-endif.
