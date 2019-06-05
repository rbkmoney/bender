-module(bender_handler).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("bender_proto/include/bender_thrift.hrl").

-include("bender_internal.hrl").

-type woody_context() :: woody_context:ctx().

-type external_id()            :: bender_thrift:'ExternalID'().
-type schema()                 :: bender:schema().
-type user_context()           :: msgpack_thrift:'Value'().
-type generate_id_result()     :: bender_thrift:'GenerationResult'().
-type get_internal_id_result() :: bender_thrift:'GetInternalIDResult'().

-spec handle_function(woody:func(), woody:args(), woody_context(), woody:options()) ->
    {ok, woody:result()}.

handle_function(Func, Args, WoodyCtx, Opts) ->
    scoper:scope(bender,
        fun() -> handle_function_(Func, Args, WoodyCtx, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), woody_context(), woody:options()) ->
    {ok, woody:result()}.

handle_function_('GenerateID', [ExternalID, Schema, UserCtx], WoodyCtx, _Opts) ->
    scoper:add_meta(#{
        external_id => ExternalID
    }),
    generate_id(ExternalID, Schema, UserCtx, WoodyCtx);

handle_function_('GetInternalID', [ExternalID], WoodyCtx, _Opts) ->
    scoper:add_meta(#{
        external_id => ExternalID
    }),
    get_internal_id(ExternalID, WoodyCtx).

-spec generate_id(external_id(), bender_thrift:'GenerationSchema'(), user_context(), woody_context()) ->
    {ok, generate_id_result()} | no_return().

generate_id(ExternalID, {constant, #bender_ConstantSchema{} = Schema}, UserCtx, WoodyCtx) ->
    NewInternalID = Schema#bender_ConstantSchema.internal_id,
    Constant      = #constant{internal_id = NewInternalID},
    bind(ExternalID, Constant, UserCtx, WoodyCtx);

generate_id(ExternalID, {sequence, #bender_SequenceSchema{} = Schema}, UserCtx, WoodyCtx) ->
    SequenceID = Schema#bender_SequenceSchema.sequence_id,
    Minimum    = Schema#bender_SequenceSchema.minimum,
    Sequence   = #sequence{id = SequenceID, minimum = Minimum},
    bind(ExternalID, Sequence, UserCtx, WoodyCtx);

generate_id(ExternalID, {snowflake, #bender_SnowflakeSchema{}}, UserCtx, WoodyCtx) ->
    bind(ExternalID, snowflake, UserCtx, WoodyCtx);

generate_id(_ExternalID, Schema, _UserCtx, _WoodyCtx) ->
    erlang:error({unknown_schema, Schema}).

-spec bind(external_id(), schema(), user_context(), woody_context()) ->
    {ok, generate_id_result()} | no_return().

bind(ExternalID, Schema, UserCtx, WoodyCtx) ->
    {ok, InternalID, PrevUserCtx} = bender_generator:bind(ExternalID, Schema, UserCtx, WoodyCtx),
    Result = #bender_GenerationResult{
        internal_id = InternalID,
        context     = PrevUserCtx
    },
    {ok, Result}.

-spec get_internal_id(external_id(), woody_context()) ->
    {ok, get_internal_id_result()} | no_return().

get_internal_id(ExternalID, WoodyCtx) ->
    try
        {ok, InternalID, UserCtx} = bender_generator:get_internal_id(ExternalID, WoodyCtx),
        Result = #bender_GetInternalIDResult{
            internal_id = InternalID,
            context = UserCtx
        },
        {ok, Result}
    catch
        throw:{not_found, ExternalID} ->
            throw(#bender_InternalIDNotFound{})
    end.
