-module(generator_handler).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("bender_proto/include/bender_thrift.hrl").

-include("bender_internal.hrl").

-type woody_context() :: woody_context:ctx().

-type schema()                 :: bender:schema().
-type generate_id_result()     :: bender_thrift:'GeneratedID'().

-spec handle_function(woody:func(), woody:args(), woody_context(), woody:options()) ->
    {ok, woody:result()}.

handle_function(Func, Args, WoodyCtx, Opts) ->
    scoper:scope(bender,
        fun() -> handle_function_(Func, Args, WoodyCtx, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), woody_context(), woody:options()) ->
    {ok, woody:result()}.

handle_function_('GenerateID', [Schema], WoodyCtx, _Opts) ->
    generate_id(Schema, WoodyCtx).

-spec generate_id(bender_thrift:'GenerationSchema'(), woody_context()) ->
    {ok, generate_id_result()} | no_return().

generate_id({constant, #bender_ConstantSchema{} = Schema}, WoodyCtx) ->
    NewInternalID = Schema#bender_ConstantSchema.internal_id,
    Constant      = #constant{internal_id = NewInternalID},
    generate(Constant, WoodyCtx);

generate_id({sequence, #bender_SequenceSchema{} = Schema}, WoodyCtx) ->
    SequenceID = Schema#bender_SequenceSchema.sequence_id,
    Minimum    = Schema#bender_SequenceSchema.minimum,
    Sequence   = #sequence{id = SequenceID, minimum = Minimum},
    generate(Sequence, WoodyCtx);

generate_id({snowflake, #bender_SnowflakeSchema{}}, WoodyCtx) ->
    generate(snowflake, WoodyCtx);

generate_id(Schema, _WoodyCtx) ->
    erlang:error({unknown_schema, Schema}).

-spec generate(schema(), woody_context()) ->
    {ok, generate_id_result()} | no_return().

generate(Schema, WoodyCtx) ->
    case bender_generator:generate(Schema, WoodyCtx) of
        {ID, IntegerID} ->
            {ok, #bender_GeneratedID{
                id = ID,
                integer_id = IntegerID
            }};
        ID ->
            {ok, #bender_GeneratedID{
                id = ID
            }}
    end.
