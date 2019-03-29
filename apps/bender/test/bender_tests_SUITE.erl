-module(bender_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([generate_snowflake/1]).

-include_lib("bender_proto/include/bender_thrift.hrl").

-type config() :: [{atom(), term()}].

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [atom()].
all() ->
    [
        generate_snowflake
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps = genlib_app:start_application_with(lager, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, [debug, {lager_logstash_formatter, []}]}
        ]}
    ]) ++ genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_lager}
    ]) ++ genlib_app:start_application_with(bender, [
        {automaton, #{
            path          => <<"/v1/stateproc/bender_snowflake">>,
            schema        => machinery_mg_schema_generic,
            url           => <<"http://machinegun:8022/v1/automaton">>,
            event_handler => scoper_woody_event_handler
        }}
    ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> term().

end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_testcase(atom(), config()) -> config().

init_per_testcase(_Name, C) ->
    Client = bender_client:new(),
    [{client, Client} | C].

-spec end_per_testcase(atom(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

-spec generate_snowflake(config()) -> term().

generate_snowflake(C) ->
    Client     = proplists:get_value(client, C),
    ExternalID = bender_utils:unique_id(),
    Schema     = {snowflake, #bender_SnowflakeSchema{}},
    UserData   = {bin, <<"breaking nudes">>},
    #bender_GenerationResult{
        internal_id = InternalID,
        context = undefined
    } = bender_client:generate_id(ExternalID, Schema, UserData, Client),
    #bender_GenerationResult{
        internal_id = InternalID,
        context = UserData
    } = bender_client:generate_id(ExternalID, Schema, UserData, Client).
