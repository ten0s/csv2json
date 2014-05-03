-ifndef(originator_hrl).
-define(originator_hrl, 1).

-record(address, {
    addr                      :: {string, string()},
    ton                       :: {integer, integer()},
    npi                       :: {integer, integer()}
}).

-record(originator, {
    id                        :: {integer, integer()},
    address                   :: #address{},
    description               :: {string, string()},
    state                     :: {string, string()},
    is_default                :: {boolean, boolean()}
}).

-record(originator_ref, {
    customer_id               :: {string, string()},
    originator                :: #originator{}
}).

-endif.
