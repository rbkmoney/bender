-module(bender_utils).

-export([unique_id/0]).

-spec unique_id() ->
    binary().

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).
