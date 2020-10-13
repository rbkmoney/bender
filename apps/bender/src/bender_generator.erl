-module(bender_generator).

%% API

-export([bind/4]).
-export([generate/2]).
-export([get_internal_id/2]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).

-type external_id() :: binary().
-type internal_id() :: binary() | {binary(), pos_integer()}.

-type schema() :: bender:schema().
-type user_context() :: msgpack_thrift:'Value'() | undefined.
-type state() :: #{
    internal_id := internal_id(),
    user_context := user_context()
}.

-type woody_context() :: woody_context:ctx().

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(_, state()).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result(A) :: machinery:result(none(), A).

-include("bender_internal.hrl").

-define(NS, bender_generator).

%%% API

-spec bind(external_id(), schema(), user_context(), woody_context()) ->
    {ok, internal_id(), user_context()} | no_return().
bind(ExternalID, Schema, UserCtx, WoodyCtx) ->
    InternalID = generate(Schema, WoodyCtx),
    case start(ExternalID, InternalID, UserCtx, WoodyCtx) of
        ok ->
            {ok, InternalID, undefined};
        {error, exists} ->
            get_internal_id(ExternalID, WoodyCtx)
    end.

-spec get_internal_id(external_id(), woody_context()) -> {ok, internal_id(), user_context()} | no_return().
get_internal_id(ExternalID, WoodyCtx) ->
    case machinery:get(?NS, ExternalID, get_backend(WoodyCtx)) of
        {ok, Machine} ->
            #{
                internal_id := InternalID,
                user_context := UserCtx
            } = get_machine_state(Machine),
            {ok, InternalID, UserCtx};
        {error, notfound} ->
            throw({not_found, ExternalID})
    end.

%%% Machinery callbacks

-spec init(args({internal_id(), user_context()}), machine(), handler_args(), handler_opts()) -> result(state()).
init({InternalID, UserCtx}, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        aux_state => #{
            internal_id => InternalID,
            user_context => UserCtx
        }
    }.

-spec process_call(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) -> no_return().
process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(repair).

%%% Internal functions

-spec start(external_id(), internal_id(), user_context(), woody_context()) -> ok | {error, exists}.
start(ExternalID, InternalID, UserCtx, WoodyCtx) ->
    machinery:start(?NS, ExternalID, {InternalID, UserCtx}, get_backend(WoodyCtx)).

-spec get_machine_state(machine()) -> state().
get_machine_state(#{aux_state := State}) ->
    State.

-spec get_backend(woody_context()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    bender_utils:get_backend(generator, WoodyCtx).

-spec not_implemented(any()) -> no_return().
not_implemented(What) ->
    erlang:error({not_implemented, What}).

-spec generate(schema(), woody_context()) -> internal_id().
generate(snowflake, _WoodyCtx) ->
    <<IntegerID:64>> = snowflake:new(),
    ID = genlib_format:format_int_base(IntegerID, 62),
    {ID, IntegerID};
generate(#constant{internal_id = InternalID}, _WoodyCtx) ->
    InternalID;
generate(#sequence{id = SequenceID, minimum = Minimum}, WoodyCtx) ->
    {ok, IntegerID} = bender_sequence:get_next(SequenceID, Minimum, WoodyCtx),
    {integer_to_binary(IntegerID), IntegerID}.
