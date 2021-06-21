-module(generator_client).

-export([new/0]).
-export([generate_id/2]).

-type client() :: woody_context:ctx().

-type schema() :: bender_thrift:'GenerationSchema'().

-define(RETRY_STATEGY, {linear, 5, 1000}).

%%% API

-spec new() -> client().
new() ->
    woody_context:new().

-spec generate_id(schema(), client()) -> woody:result() | no_return().
generate_id(Schema, Client) ->
    call('GenerateID', [Schema], Client).

%%% Internal functions

-spec call(atom(), list(), client()) -> woody:result() | no_return().
call(Function, Args, Client) ->
    Call = {{bender_thrift, 'Generator'}, Function, Args},
    Opts = #{
        url => <<"http://bender:8022/v1/generator">>,
        event_handler => scoper_woody_event_handler,
        transport_opts => #{
            max_connections => 10000
        }
    },
    call(Call, Opts, Client, ?RETRY_STATEGY).

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
