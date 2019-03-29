-module(bender_handler).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("bender_proto/include/bender_thrift.hrl").

-type woody_context() :: woody_context:ctx().

-type external_id()  :: bender_thrift:'ExternalID'().
-type schema()       :: bender_thrift:'GenerationSchema'().
-type user_context() :: msgpack_thrift:'Value'().
-type result()       :: bender_thrift:'GenerationResult'().

-spec handle_function(woody:func(), woody:args(), woody_context(), woody:options()) ->
    {ok, woody:result()}.

handle_function(Func, Args, Context, Opts) ->
    scoper:scope(bender,
        fun() -> handle_function_(Func, Args, Context, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), woody_context(), woody:options()) ->
    {ok, woody:result()}.

handle_function_('GenerateID', [ExternalID, Schema, Data], Context, _Opts) ->
    scoper:add_meta(#{
        external_id => ExternalID,
        schema => Schema,
        context => Data
    }),
    generate_id(ExternalID, Schema, Data, Context).

-spec generate_id(external_id(), schema(), user_context(), woody_context()) ->
    {ok, result()} | no_return().

generate_id(ExternalID, {snowflake, #bender_SnowflakeSchema{}}, Data, Context) ->
    {ok, InternalID, PrevData} = bender_snowflake:bind(ExternalID, Data, Context),
    Result = #bender_GenerationResult{
        internal_id = InternalID,
        context     = PrevData
    },
    {ok, Result};
generate_id(_ExternalID, Schema, _Data, _Context) ->
    erlang:error({unknown_schema, Schema}).