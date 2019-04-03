-module(bender_client).

-export([new/0]).
-export([generate_id/4]).

-type client() :: woody_context:ctx().

-type external_id()  :: bender_thrift:'ExternalID'().
-type schema()       :: bender_thrift:'GenerationSchema'().
-type user_context() :: msgpack_thrift:'Value'().

%%% API

-spec new() ->
    client().

new() ->
    woody_context:new().

-spec generate_id(external_id(), schema(), user_context(), client()) ->
    woody:result() | no_return().

generate_id(ExternalID, Schema, UserCtx, Client) ->
    call('GenerateID', [ExternalID, Schema, UserCtx], Client).

%%% Internal functions

-spec call(atom(), list(), client()) ->
    woody:result() | no_return().

call(Function, Args, Client) ->
    Call = {{bender_thrift, 'Bender'}, Function, Args},
    Opts = #{
        url => <<"http://bender:8022/v1/bender">>,
        event_handler => scoper_woody_event_handler
    },
    case woody_client:call(Call, Opts, Client) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.
