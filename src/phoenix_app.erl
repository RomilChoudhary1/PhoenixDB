-module(phoenix_app).

-behaviour(application).

-export([start/2, stop/1]).
-include("phoenix_constants.hrl").

-define(LAGER_ATTRS, [{type, startup}]).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    %% call wallclock time first, so that counters are reset
    %% so on subsequent calls the Total time returned by
    %% erlang:statistics(wall_clock) can be used as uptime.
    phoenix_util:node_uptime(second),

    %% setup caches
%%    {ok, Caches} = application:get_env(?APPLICATION_NAME, caches),
%%    lists:foreach(fun({CacheName, CacheOptions}) ->
%%        phoenix_cache_util:cache_start_link(
%%            CacheOptions, CacheName)
%%                  end, Caches),

    %% Create ets tables
    ETSTables = phoenix_ets_store:get_all_ets_table(),
    lists:foreach(fun({TableName, _}) ->
        phoenix_ets_store:create_ets_table(TableName)
                  end, ETSTables),

    PrivDir = code:priv_dir(?APPLICATION_NAME),
    %PrivDir = "priv",
    lager:debug(?LAGER_ATTRS, "[~p] ~p detected PrivDir=~p",
        [self(), ?MODULE, PrivDir]),
    HttpRestConfig = application:get_env(?APPLICATION_NAME, http_rest, []),
    Port = proplists:get_value(port, HttpRestConfig, ?DEFAULT_HTTP_PORT),
    Routes = get_routes(),
    start_http_server(http, PrivDir, Port, HttpRestConfig, Routes),
    MonitorRoutes = get_monitor_routes(),
    HttpMonitorConfig = application:get_env(?APPLICATION_NAME, http_monitor, []),
    MonitorPort = proplists:get_value(port, HttpMonitorConfig, ?DEFAULT_MONITOR_HTTP_PORT),

    start_http_server(httpmonitor, PrivDir, MonitorPort, HttpMonitorConfig, MonitorRoutes),

    %% set httpc options
    HttpClientSetOptions = phoenix_config_util:generic_http_client_set_options(),
    lager:info("[~p] ~p HttpClientSetOptions = ~p", [self(), ?MODULE, HttpClientSetOptions]),
    httpc:set_options(HttpClientSetOptions),
    %% start supervisor
    phoenix_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_http_server(HttpName, PrivDir, Port, HttpRestConfig, Routes) ->
    Dispatch = cowboy_router:compile(Routes),
    NrListeners = proplists:get_value(nr_listeners,
        HttpRestConfig,
        ?DEFAULT_HTTP_NR_LISTENERS),
    Backlog = proplists:get_value(backlog,
        HttpRestConfig,
        ?DEFAULT_HTTP_BACKLOG),
    MaxConnections = proplists:get_value(max_connections,
        HttpRestConfig,
        ?DEFAULT_HTTP_MAX_CONNECTIONS),
    %% Important: max_keepalive is only available in cowboy 2
    MaxKeepAlive = proplists:get_value(max_keepalive,
        HttpRestConfig,
        ?DEFAULT_MAX_HTTP_KEEPALIVES),
    IsSsl = proplists:get_value(ssl, HttpRestConfig,
        ?DEFAULT_HTTP_IS_SSL_ENABLED),
    IdealTimeOut = proplists:get_value(idle_timeout, HttpRestConfig,
        ?DEFAULT_HTTP_IDEAL_TIME_OUT),
    LingerTimeout = proplists:get_value(linger_timeout, HttpRestConfig,
        ?DEFAULT_HTTP_LINGER_TIME_OUT),
    InActivityTimeout = proplists:get_value(inactivity_timeout, HttpRestConfig,
        ?DEFAULT_HTTP_INACTIVITY_TIME_OUT),
    case IsSsl of
        true ->
            {ok, _} = cowboy:start_tls(HttpName, [
                {port, Port},
                {num_acceptors, NrListeners},
                {backlog, Backlog},
                {max_connections, MaxConnections},
                %% {cacertfile, PrivDir ++ "/ssl/ca-chain.cert.pem"},
                {certfile, PrivDir ++ "/ssl/cert.pem"},
                {keyfile, PrivDir ++ "/ssl/key.pem"},
                {versions, ['tlsv1.2']},
                {ciphers, ?DEFAULT_HTTPS_CIPHERS}
            ],
                #{env => #{dispatch => Dispatch},
                    %% TODO: stream_handlers => [stream_http_rest_log_handler],
                    %%onresponse => fun log_utils:req_log/4,
                    idle_timeout => IdealTimeOut,
                    linger_timeout => LingerTimeout,
                    inactivity_timeout => InActivityTimeout,
                    max_keepalive => MaxKeepAlive});
        false ->
            {ok, _} = cowboy:start_clear(HttpName, [
                {port, Port},
                {num_acceptors, NrListeners},
                {backlog, Backlog},
                {max_connections, MaxConnections}
            ],
                #{env => #{dispatch => Dispatch},
                    %% TODO: stream_handlers => [stream_http_rest_log_handler],
                    %%onresponse => fun log_utils:req_log/4,
                    idle_timeout => IdealTimeOut,
                    linger_timeout => LingerTimeout,
                    inactivity_timeout => InActivityTimeout,
                    max_keepalive => MaxKeepAlive})
    end.

%% @private get routes for cowboy dispatch
-spec get_routes() -> list().
get_routes() ->
%%    PrivDir = code:priv_dir(?APPLICATION_NAME),
    [{'_',
        [
            %%%%%%%%%%%%%%%%%%%%%%%%%% PHOENIX LEVELDB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            {"/phoenix/:id", phoenix_leveldb_handler, []},
            {"/phoenix/:shardKey/:id", phoenix_leveldb_handler, [shard_handler]},
            {"/stats", phoenix_stats_handler, []}
        ]}].

get_monitor_routes() ->
    [{'_',
        [
            {"/stats",
                phoenix_stats_handler, []}]
    }].