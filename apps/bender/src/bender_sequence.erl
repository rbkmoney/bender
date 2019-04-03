-module(bender_sequence).

%% API

-export([get_current/2]).
-export([get_next/2]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).

-type id() :: binary().

-export_types([id/0]).

-type woody_context() :: woody_context:ctx().

-type args()         :: machinery:args(_).
-type machine()      :: machinery:machine(_, _).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).

-define(NS, bender_sequence).

%%% API

-spec get_current(id(), woody_context()) ->
    {ok, non_neg_integer()} | {error, notfound}.

get_current(SequenceID, Context) ->
    case get_state(SequenceID, Context) of
        {ok, #{<<"value">> := Value}} ->
            {ok, Value};
        _ ->
            {error, notfound}
    end.

-spec get_next(id(), woody_context()) ->
    {ok, non_neg_integer()}.

get_next(SequenceID, Context) ->
    ok = ensure_started(SequenceID, Context),
    call(SequenceID, get_next, Context).

%%% Machinery callbacks

-spec init(args(), machine(), handler_args(), handler_opts()) ->
    machinery:result(_, _).

init([], _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        aux_state => #{
            <<"value">> => 0
        }
    }.

-spec process_call(args(), machine(), handler_args(), handler_opts()) ->
    {machinery:response(_), machinery:result(_, _)} | no_return().

process_call(get_next, Machine, _HandlerArgs, _HandlerOpts) ->
    #{aux_state := State} = Machine,
    #{<<"value">> := Value} = State,
    NewValue = Value + 1,
    NewState = State#{<<"value">> => NewValue},
    {NewValue, #{aux_state => NewState}};

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

-spec start(id(), woody_context()) ->
    ok | {error, exists}.

start(SequenceID, Context) ->
    machinery:start(?NS, SequenceID, [], get_backend(Context)).

-spec ensure_started(id(), woody_context()) ->
    ok | no_return().

ensure_started(SequenceID, Context) ->
    case start(SequenceID, Context) of
        ok ->
            ok;
        {error, exists} ->
            ok
    end.

-spec call(id(), machinery:args(_), woody_context()) ->
    {ok, machinery:response(_)} | {error, notfound}.

call(SequenceID, Msg, Context) ->
    machinery:call(?NS, SequenceID, Msg, get_backend(Context)).

-spec get_state(id(), woody_context()) ->
    {ok, term()} | {error, notfound}.

get_state(SequenceID, Context) ->
    case machinery:get(?NS, SequenceID, get_backend(Context)) of
        {ok, Machine} ->
            State = get_machine_state(Machine),
            {ok, State};
        {error, notfound} = Error ->
            Error
    end.

-spec get_machine_state(machinery:machine(_, _)) ->
    term().

get_machine_state(#{aux_state := State}) ->
    State.

-spec get_backend(woody_context()) ->
    machinery_mg_backend:backend().

get_backend(Context) ->
    Automaton = genlib_app:env(bender, sequence, #{}),
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
