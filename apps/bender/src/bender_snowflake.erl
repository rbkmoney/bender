-module(bender_snowflake).

%% API

-export([bind/3]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).

-type external_id()  :: binary().
-type internal_id()  :: binary().
-type user_context() :: msgpack_thrift:'Value'() | undefined.
-type data()         :: map().

-type woody_context() :: woody_context:ctx().

-type args()         :: machinery:args(_).
-type machine()      :: machinery:machine(_, _).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).

-define(NS, bender_snowflake).

%%% API

-spec bind(external_id(), user_context(), woody_context()) ->
    {ok, internal_id(), user_context()}.

bind(ExternalID, Data, Context) ->
    case start(ExternalID, Data, Context) of
        ok ->
            {ok, InternalID, _} = get(ExternalID, Context),
            {ok, InternalID, undefined};
        {error, exists} ->
            get(ExternalID, Context)
    end.

%%% Machinery callbacks

-spec init(args(), machine(), handler_args(), handler_opts()) ->
    machinery:result(_, data()).

init(Data, _Machine, _HandlerArgs, _HandlerOpts) ->
    InternalID = bender_utils:unique_id(),
    #{
        aux_state => #{
            <<"internal_id">> => InternalID,
            <<"data">>        => Data
        }
    }.

-spec process_call(args(), machine(), handler_args(), handler_opts()) ->
    no_return().

process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    no_return().

process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(), machine(), handler_args(), handler_opts()) ->
    no_return().

process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(repair).

%%% Internal functions

-spec start(external_id(), machinery:args(user_context()), woody_context()) ->
    ok | {error, exists}.

start(ExternalID, Data, Context) ->
    machinery:start(?NS, ExternalID, Data, get_backend(Context)).

-spec get(external_id(), woody_context()) ->
    {ok, data()} | {error, notfound}.

get(ExternalID, Context) ->
    case machinery:get(?NS, ExternalID, get_backend(Context)) of
        {ok, Machine} ->
            #{
                <<"internal_id">> := InternalID,
                <<"data">> := Data
            } = get_machine_state(Machine),
            {ok, InternalID, Data};
        {error, notfound} = Error ->
            Error
    end.

-spec get_machine_state(machinery:machine(_, data())) ->
    any().

get_machine_state(#{aux_state := Data}) ->
    Data.

-spec get_backend(woody_context()) ->
    machinery_mg_backend:backend().

get_backend(Context) ->
    Automaton = genlib_app:env(bender, automaton, #{}),
    machinery_mg_backend:new(Context, #{
        client => get_woody_client(Automaton),
        schema => maps:get(schema, Automaton, machinery_mg_schema_generic)
    }).

-spec get_woody_client(map()) ->
    machinery_mg_client:woody_client().

get_woody_client(Automaton) ->
    #{
        url => maps:get(url, Automaton),
        event_handler => maps:get(event_handler, Automaton)
        % FIXME: should we add optional transport_opts here?
    }.

-spec not_implemented(any()) ->
    no_return().

not_implemented(What) ->
    erlang:error({not_implemented, What}).
