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

-type args(T)        :: machinery:args(T).
-type machine()      :: machinery:machine(_, state()).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result(A)      :: machinery:result(none(), A).
-type response(T)    :: machinery:response(T).

-type value() :: non_neg_integer().
-type state() :: #{
    value := value()
}.

-define(NS, bender_sequence).

-define(initial_value, 0).

%%% API

-spec get_current(id(), woody_context()) ->
    {ok, value()} | {error, notfound}.

get_current(SequenceID, WoodyCtx) ->
    case get_state(SequenceID, WoodyCtx) of
        {ok, State} ->
            {ok, get_value(State)};
        _ ->
            {error, notfound}
    end.

-spec get_next(id(), woody_context()) ->
    {ok, value()}.

get_next(SequenceID, WoodyCtx) ->
    ok = ensure_started(SequenceID, WoodyCtx),
    call(SequenceID, get_next, WoodyCtx).

%%% Machinery callbacks

-spec init(args([]), machine(), handler_args(), handler_opts()) ->
    result(state()).

init([], _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        aux_state => #{
            value => ?initial_value
        }
    }.

-spec process_call(args(get_next | any()), machine(), handler_args(), handler_opts()) ->
    {response(value()), result(state())} | no_return().

process_call(get_next, Machine, _HandlerArgs, _HandlerOpts) ->
    State    = get_machine_state(Machine),
    Value    = get_value(State),
    NewValue = Value + 1,
    NewState = set_value(State, NewValue),
    {NewValue, #{aux_state => NewState}};

process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    no_return().

process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) ->
    no_return().

process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(repair).

%%% Internal functions

-spec start(id(), woody_context()) ->
    ok | {error, exists}.

start(SequenceID, WoodyCtx) ->
    machinery:start(?NS, SequenceID, [], get_backend(WoodyCtx)).

-spec ensure_started(id(), woody_context()) ->
    ok | no_return().

ensure_started(SequenceID, WoodyCtx) ->
    case start(SequenceID, WoodyCtx) of
        ok ->
            ok;
        {error, exists} ->
            ok
    end.

-spec call(id(), args(_), woody_context()) ->
    {ok, response(_)} | {error, notfound}.

call(SequenceID, Msg, WoodyCtx) ->
    machinery:call(?NS, SequenceID, Msg, get_backend(WoodyCtx)).

-spec get_state(id(), woody_context()) ->
    {ok, state()} | {error, notfound}.

get_state(SequenceID, WoodyCtx) ->
    case machinery:get(?NS, SequenceID, get_backend(WoodyCtx)) of
        {ok, Machine} ->
            State = get_machine_state(Machine),
            {ok, State};
        {error, notfound} = Error ->
            Error
    end.

-spec get_machine_state(machine()) ->
    state().

get_machine_state(#{aux_state := State}) ->
    State.

-spec get_backend(woody_context()) ->
    machinery_mg_backend:backend().

get_backend(WoodyCtx) ->
    bender_utils:get_backend(sequence, WoodyCtx).

-spec not_implemented(any()) ->
    no_return().

not_implemented(What) ->
    erlang:error({not_implemented, What}).

-spec get_value(state()) ->
    value().

get_value(#{value := Value}) ->
    Value.

-spec set_value(state(), value()) ->
    state().

set_value(State, Value) ->
    State#{value => Value}.