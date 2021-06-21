-module(bender_sequence).

%% API

-export([get_current/2]).
-export([get_next/2]).
-export([get_next/3]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).

-type id() :: binary().
-type minimum() :: integer() | undefined.

-export_type([id/0]).
-export_type([minimum/0]).

-type woody_context() :: woody_context:ctx().

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(_, state()).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result(A) :: machinery:result(none(), A).
-type response(T) :: machinery:response(T).

-type value() :: non_neg_integer().
-type state() :: #{
    value := value()
}.

-type init_args() :: #{
    initial_value => value()
}.

-define(NS, bender_sequence).

-define(DEFAULT_INITIAL_VALUE, 1).

%%% API

-spec get_current(id(), woody_context()) -> {ok, value()} | {error, notfound}.
get_current(SequenceID, WoodyCtx) ->
    case get_state(SequenceID, WoodyCtx) of
        {ok, State} ->
            {ok, get_value(State)};
        _ ->
            {error, notfound}
    end.

-spec get_next(id(), woody_context()) -> {ok, value()}.
get_next(SequenceID, WoodyCtx) ->
    get_next(SequenceID, ?DEFAULT_INITIAL_VALUE, WoodyCtx).

-spec get_next(id(), minimum(), woody_context()) -> {ok, value()}.
get_next(SequenceID, undefined, WoodyCtx) ->
    get_next(SequenceID, ?DEFAULT_INITIAL_VALUE, WoodyCtx);
get_next(SequenceID, Minimum, WoodyCtx) ->
    Args = #{initial_value => Minimum},
    case start(SequenceID, Args, WoodyCtx) of
        ok ->
            {ok, Minimum};
        {error, exists} ->
            call(SequenceID, {get_next, Minimum}, WoodyCtx)
    end.

%%% Machinery callbacks

-spec init(args(init_args()), machine(), handler_args(), handler_opts()) -> result(state()).
init(Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        aux_state => #{
            value => maps:get(initial_value, Args, ?DEFAULT_INITIAL_VALUE)
        }
    }.

-spec process_call(args({get_next, integer()} | any()), machine(), handler_args(), handler_opts()) ->
    {response(value()), result(state())} | no_return().
process_call({get_next, Minimum}, Machine, _HandlerArgs, _HandlerOpts) ->
    State = get_machine_state(Machine),
    Value = get_value(State),
    NewValue = erlang:max(Value + 1, Minimum),
    NewState = set_value(State, NewValue),
    {NewValue, #{aux_state => NewState}};
process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) -> no_return().
process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(repair).

%%% Internal functions

-spec start(id(), init_args(), woody_context()) -> ok | {error, exists}.
start(SequenceID, Args, WoodyCtx) ->
    machinery:start(?NS, SequenceID, Args, get_backend(WoodyCtx)).

-spec call(id(), args(_), woody_context()) -> {ok, response(_)} | {error, notfound}.
call(SequenceID, Msg, WoodyCtx) ->
    machinery:call(?NS, SequenceID, Msg, get_backend(WoodyCtx)).

-spec get_state(id(), woody_context()) -> {ok, state()} | {error, notfound}.
get_state(SequenceID, WoodyCtx) ->
    case machinery:get(?NS, SequenceID, get_backend(WoodyCtx)) of
        {ok, Machine} ->
            State = get_machine_state(Machine),
            {ok, State};
        {error, notfound} = Error ->
            Error
    end.

-spec get_machine_state(machine()) -> state().
get_machine_state(#{aux_state := State}) ->
    State.

-spec get_backend(woody_context()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    bender_utils:get_backend(sequence, WoodyCtx).

-spec not_implemented(any()) -> no_return().
not_implemented(What) ->
    erlang:error({not_implemented, What}).

-spec get_value(state()) -> value().
get_value(#{value := Value}) ->
    Value.

-spec set_value(state(), value()) -> state().
set_value(State, Value) ->
    State#{value => Value}.
