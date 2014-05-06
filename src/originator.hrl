-ifndef(originator_hrl).
-define(originator_hrl, 1).

-include("address.hrl").

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
