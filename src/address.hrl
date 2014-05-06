-ifndef(address_hrl).
-define(address_hrl, 1).

-record(address, {
    addr                      :: {string, string()},
    ton                       :: {integer, integer()},
    npi                       :: {integer, integer()}
}).

-endif.
