-module(bender_client).

-export([new/0]).
-export([generate_id/4]).
-export([get_internal_id/2]).

-type client() :: woody_context:ctx().

-type external_id() :: bender_thrift:'ExternalID'().
-type schema() :: bender_thrift:'GenerationSchema'().
-type user_context() :: msgpack_thrift:'Value'().

-define(retry_stategy, {linear, 5, 1000}).

%%% API

-spec new() -> client().
new() ->
    woody_context:new().

-spec generate_id(external_id(), schema(), user_context(), client()) -> woody:result() | no_return().
generate_id(ExternalID, Schema, UserCtx, Client) ->
    call('GenerateID', [ExternalID, Schema, UserCtx], Client).

-spec get_internal_id(external_id(), client()) -> woody:result() | no_return().
get_internal_id(ExternalID, Client) ->
    call('GetInternalID', [ExternalID], Client).

%%% Internal functions

-spec call(atom(), list(), client()) -> woody:result() | no_return().
call(Function, Args, Client) ->
    Call = {{bender_thrift, 'Bender'}, Function, Args},
    Opts = #{
        url => <<"http://bender:8022/v1/bender">>,
        event_handler => scoper_woody_event_handler,
        transport_opts => #{
            max_connections => 10000
        }
    },
    call(Call, Opts, Client, ?retry_stategy).

call(Call, Opts, Client, Retry) ->
    try
        do_call(Call, Opts, Client)
    catch
        error:{woody_error, {_Source, Class, _Details}} = Error when
            Class =:= resource_unavailable orelse Class =:= result_unknown
        ->
            NextRetry = next_retry(Retry, Error),
            call(Call, Opts, Client, NextRetry)
    end.

do_call(Call, Opts, Client) ->
    case woody_client:call(Call, Opts, Client) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.

next_retry(Retry, Error) ->
    retry_step(genlib_retry:next_step(Retry), Error).

retry_step(finish, Error) ->
    erlang:error(Error);
retry_step({wait, Timeout, Retry}, _) ->
    ok = timer:sleep(Timeout),
    Retry.
