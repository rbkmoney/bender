-module(generator_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([constant/1]).
-export([sequence/1]).
-export([sequence_minimum/1]).
-export([snowflake/1]).


-include_lib("bender_proto/include/bender_thrift.hrl").

-type config() :: [{atom(), term()}].
-type group_name() :: atom().
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() ->
    [atom()].

all() ->
    [
        {group, main}
    ].

-define(parallel_workers, 100).

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {main, [parallel], [
            {group, constant},
            {group, sequence},
            sequence_minimum,
            {group, snowflake}
        ]},
        {constant, [parallel], [ constant || _ <- lists:seq(1, ?parallel_workers) ]},
        {sequence, [parallel], [ sequence || _ <- lists:seq(1, ?parallel_workers) ]},
        {snowflake, [parallel], [ snowflake || _ <- lists:seq(1, ?parallel_workers) ]}
    ].

-spec init_per_suite(config()) ->
    config().

init_per_suite(C) ->
    Apps = genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_logger}
    ]) ++ genlib_app:start_application_with(bender, [
        {generator, #{
            path          => <<"/v1/stateproc/bender_generator">>,
            schema        => machinery_mg_schema_generic,
            url           => <<"http://machinegun:8022/v1/automaton">>,
            event_handler => scoper_woody_event_handler,
            transport_opts => #{
                max_connections => 1000
            }
        }},
        {sequence, #{
            path          => <<"/v1/stateproc/bender_sequence">>,
            schema        => machinery_mg_schema_generic,
            url           => <<"http://machinegun:8022/v1/automaton">>,
            event_handler => scoper_woody_event_handler,
            transport_opts => #{
                max_connections => 1000
            }
        }},
        {protocol_opts, #{
            timeout => 60000
        }},
        {transport_opts, #{
            max_connections => 10000,
            num_acceptors => 100
        }}
    ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) ->
    ok.

end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_testcase(atom(), config()) ->
    config().

init_per_testcase(_Name, C) ->
    Client = generator_client:new(),
    [{client, Client} | C].

-spec end_per_testcase(atom(), config()) ->
    config().

end_per_testcase(_Name, _C) ->
    ok.

-spec constant(config()) ->
    ok.

constant(C) ->
    Client     = get_client(C),
    InternalID = bender_utils:unique_id(),
    Schema     = {constant, #bender_ConstantSchema{internal_id = InternalID}},
    InternalID = generate(Schema, Client),
    ok.

-spec sequence(config()) ->
    ok.

sequence(C) ->
    Client       = get_client(C),
    SequenceID   = bender_utils:unique_id(),
    Schema       = {sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
    {<<"1">>, 1} = generate(Schema, Client),
    {<<"2">>, 2} = generate(Schema, Client),
    {<<"3">>, 3} = generate(Schema, Client),
    ok.

-spec sequence_minimum(config()) ->
    ok.

sequence_minimum(C) ->
    Client       = get_client(C),
    SequenceID   = bender_utils:unique_id(),
    Schema1      = {sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
    {<<"1">>, 1} = generate(Schema1, Client),
    Schema2      = {sequence, #bender_SequenceSchema{sequence_id = SequenceID, minimum = 4}},
    {<<"4">>, 4} = generate(Schema2, Client),
    OtherSeqID   = bender_utils:unique_id(),
    Schema3      = {sequence, #bender_SequenceSchema{sequence_id = OtherSeqID, minimum = 8}},
    {<<"8">>, 8} = generate(Schema3, Client),
    ok.

-spec snowflake(config()) ->
    ok.

snowflake(C) ->
    Client     = get_client(C),
    Schema     = {snowflake, #bender_SnowflakeSchema{}},
    {_ID, _IntegerID} = generate(Schema, Client),
    ok.


%%%

get_client(C) ->
    ?config(client, C).

generate(Schema, Client) ->
    case generator_client:generate_id(Schema, Client) of
        #bender_GeneratedID{
            id = ID,
            integer_id = undefined
        } -> ID;
        #bender_GeneratedID{
            id = ID,
            integer_id = IntegerID
        } -> {ID, IntegerID}
    end.
