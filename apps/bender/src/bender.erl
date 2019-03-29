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

%% API

-spec start() ->
    {ok, _}.

start() ->
    application:ensure_all_started(?MODULE).

-spec stop() ->
    ok.

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
    {ok, Ip} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    RouteOptsEnv = genlib_app:env(?MODULE, route_opts, #{}),
    RouteOpts = RouteOptsEnv#{event_handler => scoper_woody_event_handler},
    Automaton = genlib_app:env(bender, automaton, #{}),
    Handlers = [
        {bender_snowflake, #{
            path => maps:get(path, Automaton, <<"/v1/stateproc/bender_snowflake">>),
            backend_config => #{
                schema => maps:get(schema, Automaton, machinery_mg_schema_generic)
            }
        }}
    ],
    ChildSpec = woody_server:child_spec(
        ?MODULE,
        #{
            ip            => Ip,
            port          => genlib_app:env(?MODULE, port, 8022),
            net_opts      => genlib_app:env(?MODULE, net_opts, []),
            event_handler => scoper_woody_event_handler,
            handlers      => [],
            additional_routes => [
                erl_health_handle:get_route(HealthCheckers) |
                get_bender_route()
            ] ++ machinery_mg_backend:get_routes(Handlers, RouteOpts)
        }
    ),
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [ChildSpec]
    }}.

-spec get_bender_route() ->
    [woody_server_thrift_http_handler:route(_)].

get_bender_route() ->
    Opts   = genlib_app:env(?MODULE, service, #{}),
    Path   = maps:get(path, Opts, <<"/v1/bender">>),
    Limits = genlib_map:get(handler_limits, Opts),
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers       => [{Path, {{bender_thrift, 'Bender'}, {bender_handler, []}}}],
        event_handler  => scoper_woody_event_handler,
        handler_limits => Limits
    })).
