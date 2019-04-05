-ifndef(__BENDER_INTERNAL_HRL__).
-define(__BENDER_INTERNAL_HRL__, included).

-record(constant, {
    internal_id :: bender_thrift:'InternalID'()
}).

-record(sequence, {
    id :: binary()
}).

-endif.