-module(bender).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%% Supervisor callbacks
-export([init/1]).

-include("bender_internal.hrl").

-type schema() :: snowflake | #constant{} | #sequence{}.

-export_type([schema/0]).

%% API

-spec start() ->
    {ok, [atom()]} | {error, {atom(), term()}}.

start() ->
    application:ensure_all_started(?MODULE).

-spec stop() ->
    ok | {error, term()}.

stop() ->
    application:stop(?MODULE).

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.

stop(_State) ->
    ok.

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    ChildSpec = woody_server:child_spec(
        ?MODULE,
        #{
            ip                => get_ip_address(),
            port              => get_port(),
            protocol_opts     => get_protocol_opts(),
            transport_opts    => get_transport_opts(),
            shutdown_timeout  => get_shutdown_timeout(),
            event_handler     => scoper_woody_event_handler,
            handlers          => [get_handler_spec()],
            additional_routes => get_routes()
        }
    ),
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [ChildSpec]
    }}.

-spec get_ip_address() ->
    inet:ip_address().

get_ip_address() ->
    {ok, Address} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    Address.

-spec get_port() ->
    inet:port_number().

get_port() ->
    genlib_app:env(?MODULE, port, 8022).

-spec get_protocol_opts() ->
    woody_server_thrift_http_handler:protocol_opts().

get_protocol_opts() ->
    genlib_app:env(?MODULE, protocol_opts, #{}).

-spec get_transport_opts() ->
    woody_server_thrift_http_handler:transport_opts().

get_transport_opts() ->
    genlib_app:env(?MODULE, transport_opts, #{}).

-spec get_shutdown_timeout() ->
    timeout().

get_shutdown_timeout() ->
    genlib_app:env(?MODULE, shutdown_timeout, 0).

-spec get_handler_spec() ->
    woody:http_handler(woody:th_handler()).

get_handler_spec() ->
    Opts = genlib_app:env(?MODULE, service, #{}),
    Path = maps:get(path, Opts, <<"/v1/bender">>),
    {Path, {
        {bender_thrift, 'Bender'},
        bender_handler
    }}.

-spec get_routes() ->
    [woody_server_thrift_http_handler:route(_)].

get_routes() ->
    RouteOptsEnv = genlib_app:env(?MODULE, route_opts, #{}),
    RouteOpts = RouteOptsEnv#{event_handler => scoper_woody_event_handler},
    Generator = genlib_app:env(bender, generator, #{}),
    Sequence = genlib_app:env(bender, sequence, #{}),
    Handlers = [
        {bender_generator, #{
            path => maps:get(path, Generator, <<"/v1/stateproc/bender_generator">>),
            backend_config => #{
                schema => maps:get(schema, Generator, machinery_mg_schema_generic)
            }
        }},
        {bender_sequence, #{
            path => maps:get(path, Sequence, <<"/v1/stateproc/bender_sequence">>),
            backend_config => #{
                schema => maps:get(schema, Sequence, machinery_mg_schema_generic)
            }
        }}
    ],
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    [erl_health_handle:get_route(HealthCheckers) |
     machinery_mg_backend:get_routes(Handlers, RouteOpts)].
