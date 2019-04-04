-module(bender_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([constant/1]).
-export([sequence/1]).
-export([snowflake/1]).

-export([different_schemas/1]).
-export([contention/1]).

-export([generator_init/1]).

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

-define(parallel_workers, 1000).
-define(contention_test_workers, 1000).

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {main, [parallel], [
            {group, constant},
            {group, sequence},
            {group, snowflake},
            {group, different_schemas},
            {group, generator_init},
            contention
        ]},
        {constant, [parallel], [ constant || _ <- lists:seq(1, ?parallel_workers) ]},
        {sequence, [parallel], [ sequence || _ <- lists:seq(1, ?parallel_workers) ]},
        {snowflake, [parallel], [ snowflake || _ <- lists:seq(1, ?parallel_workers) ]},
        {different_schemas, [parallel], [ different_schemas || _ <- lists:seq(1, ?parallel_workers) ]},
        {generator_init, [parallel], [ generator_init || _ <- lists:seq(1, ?parallel_workers) ]}
    ].

-spec init_per_suite(config()) ->
    config().

init_per_suite(C) ->
    Apps = genlib_app:start_application_with(lager, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, [error, {lager_logstash_formatter, []}]}
        ]}
    ]) ++ genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_lager}
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
    Client = bender_client:new(),
    [{client, Client} | C].

-spec end_per_testcase(atom(), config()) ->
    config().

end_per_testcase(_Name, _C) ->
    ok.

-spec constant(config()) ->
    ok.

constant(C) ->
    Client     = get_client(C),
    ExternalID = bender_utils:unique_id(),
    InternalID = bender_utils:unique_id(),
    Schema     = {constant, #bender_ConstantSchema{internal_id = InternalID}},
    UserCtx    = {bin, <<"spiel mit mir">>},
    InternalID = generate_weak(ExternalID, Schema, UserCtx, Client),
    InternalID = generate_strict(ExternalID, Schema, UserCtx, Client),
    ok.

-spec sequence(config()) ->
    ok.

sequence(C) ->
    Client     = get_client(C),
    SequenceID = bender_utils:unique_id(),
    ExternalID = bender_utils:unique_id(),
    Schema     = {sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
    UserCtx    = {bin, <<"come to daddy">>},
    <<"1">>    = generate_weak(ExternalID, Schema, UserCtx, Client),
    OtherID    = bender_utils:unique_id(),
    <<"2">>    = generate_weak(OtherID, Schema, UserCtx, Client),
    <<"1">>    = generate_strict(ExternalID, Schema, UserCtx, Client),
    ok.

-spec snowflake(config()) ->
    ok.

snowflake(C) ->
    Client     = get_client(C),
    ExternalID = bender_utils:unique_id(),
    Schema     = {snowflake, #bender_SnowflakeSchema{}},
    UserCtx    = {bin, <<"breaking nudes">>},
    InternalID = generate_weak(ExternalID, Schema, UserCtx, Client),
    InternalID = generate_strict(ExternalID, Schema, UserCtx, Client),
    ok.

-spec different_schemas(config()) ->
    ok.

different_schemas(C) ->
    Client     = get_client(C),
    ExternalID = bender_utils:unique_id(),
    Schema1    = {sequence, #bender_SequenceSchema{sequence_id = bender_utils:unique_id()}},
    UserCtx    = {bin, <<"wo bist do">>},
    InternalID = generate_weak(ExternalID, Schema1, UserCtx, Client),
    Schema2    = {snowflake, #bender_SnowflakeSchema{}},
    InternalID = generate_strict(ExternalID, Schema2, UserCtx, Client),
    Schema3    = {constant, #bender_ConstantSchema{internal_id = bender_utils:unique_id()}},
    InternalID = generate_strict(ExternalID, Schema3, UserCtx, Client),
    ok.

-spec contention(config()) ->
    ok.

contention(C) ->
    ExternalID = bender_utils:unique_id(),
    SequenceID = bender_utils:unique_id(),
    Data = [
        {{snowflake, #bender_SnowflakeSchema{}},
         bender_utils:unique_id()} ||
        _ <- lists:seq(1, ?contention_test_workers)
    ] ++ [
        {{constant, #bender_ConstantSchema{internal_id = bender_utils:unique_id()}},
         bender_utils:unique_id()} ||
        _ <- lists:seq(1, ?contention_test_workers)
    ] ++ [
        {{sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
         bender_utils:unique_id()} ||
        _ <- lists:seq(1, ?contention_test_workers)
    ],
    Generate =
        fun({Schema, UserCtx}) ->
            Client = get_client(C),
            UserCtx1 = {bin, term_to_binary({Schema, UserCtx})},
            {InternalID, PrevUserCtx} = generate(ExternalID, Schema, UserCtx1, Client),
            {{ExternalID, InternalID, PrevUserCtx}, {Schema, UserCtx}}
        end,
    Result = genlib_pmap:map(Generate, shuffle(Data)),
    [
        {{ExternalID, InternalID, undefined}, UserCtxOfWinner},
        {{ExternalID, InternalID, {bin, BinaryCtx}}, _OtherUserCtx}
    ] = lists:ukeysort(1, Result),
    UserCtxOfWinner = binary_to_term(BinaryCtx).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-spec generator_init(config()) ->
    ok.

generator_init(_C) ->
    Request = #mg_stateproc_SignalArgs{
        signal = {
            init,
            #mg_stateproc_InitSignal{
                arg = {arr, [{str, <<"tup">>}, {str, <<"snowflake">>}, {bin, <<"user context">>}]}
            }
        },
        machine = #mg_stateproc_Machine{
            ns = <<"bender_generator">>,
            id = <<"42">>,
            history = [],
            history_range = #mg_stateproc_HistoryRange{},
            aux_state = {bin, <<>>},
            timer = undefined
        }
    },
    Call = {{mg_proto_state_processing_thrift, 'Processor'}, 'ProcessSignal', [Request]},
    Options = #{
        url => <<"http://localhost:8022/v1/stateproc/bender_generator">>,
        event_handler => scoper_woody_event_handler,
        transport_opts => #{
            checkout_timeout => 1000,
            max_connections => 10000
        }
    },
    {ok, _Result} = woody_client:call(Call, Options).

%%%

get_client(C) ->
    ?config(client, C).

generate(ExternalID, Schema, UserCtx, Client) ->
    #bender_GenerationResult{
        internal_id = InternalID,
        context     = PrevUserCtx
    } = bender_client:generate_id(ExternalID, Schema, UserCtx, Client),
    {InternalID, PrevUserCtx}.

generate_strict(ExternalID, Schema, UserCtx, Client) ->
    {InternalID, UserCtx} = generate(ExternalID, Schema, UserCtx, Client),
    InternalID.

generate_weak(ExternalID, Schema, UserCtx, Client) ->
    case generate(ExternalID, Schema, UserCtx, Client) of
        {InternalID, undefined} ->
            InternalID;
        {InternalID, UserCtx} ->
            InternalID
    end.

shuffle(L) ->
    [ T || {_, T} <- lists:sort([ {rand:uniform(), E} || E <- L ]) ].
