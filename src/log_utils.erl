-module(log_utils).
-export([create_log_req/2]).
-export([req_log/4]).
-export([publish_counter/2,
         publish_gauge/2,
         publish_histogram/2]).
-export([publish_http_error_code_metrics/2]).

-include("phoenix_constants.hrl").


create_log_req(Req, Model) ->
    Method = cowboy_req:method(Req),
    RequestPath = cowboy_req:path(Req),
    QsVals = cowboy_req:qs(Req),
    otter_span_pdict_api:start({Method, Model}),
    otter_span_pdict_api:tag("node", node()),
    otter_span_pdict_api:log({path, RequestPath}),
    otter_span_pdict_api:log({qs_vals, QsVals}),
    HttpRestConfig = application:get_env(?APPLICATION_NAME,
                                         http_rest,
                                         []),
    IsLogEnabled = proplists:get_value(log_enabled, HttpRestConfig, false),
    Req#{log_enabled => IsLogEnabled,
         start_time => erlang:monotonic_time(millisecond),
         metrics_source => Model}.

%% @doc Request logs provided as a cowboy hook.
%%
%% Folsom metrics can be triggered by having the cowboy handler setting
%% a given `metrics_source' in the `meta' values of the `cowboy_req'
%% object. Counts for status codes returned will be accumulated.
%% If adding a `meta' value named `start_time', whose value is obtained
%% by calling `erlang:montonic_time(milli_seconds)',  time difference
%% calculation will be done between this time value and the end of the
%% response.
%%
%% By setting `log_enabled' to `true' in the `cowboy_req' `meta' object,
%% log lines for an HTTP status will be generated for each response.
req_log(Status, _Headers, _IoData, Req) ->
    Req1 = publish_metrics(Status, Req),
    Req2 = publish_req_logs(Status, Req1),
    Req2.

publish_metrics(Status, Req1) ->
    Type = http_status_to_metric_name(Status),
    publish_counter(Type, 1),
    case maps:get(metrics_source, Req1, undefined) of
        undefined ->
            Req1;
        Term when is_atom(Term) ->
            Method = cowboy_req:method(Req1),
            RootKey = atom_to_binary(Term, utf8),
            OrgUnitBin = phoenix_config_util:get_org_unit(),
            %% publish average cost of each api
            [{reductions, Reductions},
             {memory, Memory}] = erlang:process_info(self(), [reductions, memory]),
            Method = cowboy_req:method(Req1),
            publish_counter(<<"cost.reduction.api.", Method/binary, ".", RootKey/binary, ".", Type/binary, ".", OrgUnitBin/binary>>,
                              Reductions div 1000),
            publish_histogram(<<"cost.memory.api.", Method/binary, ".", RootKey/binary, ".", Type/binary, ".ms."/utf8, OrgUnitBin/binary>>,
                              Memory),
            %% publish regular counters
            publish_counter(<<"api.", Method/binary, ".", RootKey/binary, ".", Type/binary, ".", OrgUnitBin/binary>>, 1),
            case maps:get(start_time, Req1, undefined) of
                undefined ->
                    Req1;
                T0 ->
                    T1 = erlang:monotonic_time(milli_seconds),
                    DeltaT = T1 - T0,
                    publish_histogram(<<"api.", Method/binary, ".",
                                        RootKey/binary, ".",
                                        Type/binary, ".ms."/utf8, OrgUnitBin/binary>>, DeltaT),
                    Req1
            end
    end.

publish_req_logs(Status, Req1) ->
    case maps:get(log_enabled, Req1, false) of
        true ->
            Method = cowboy_req:method(Req1),
            Path = cowboy_req:path(Req1),
            Agent = cowboy_req:header(<<"user-agent">>, Req1, <<"">>),
            {IP, _Port} = cowboy_req:peer(Req1),
            Str = "method=~s path=~s status=~p ip=~s agent=~p",
            Args = [Method, Path, Status, format_ip(IP),
                    binary_to_list(Agent)],
            case Status of
                _ when Status < 500 -> req_logs:info(Str, Args);
                _ when Status >= 500 -> req_logs:warning(Str, Args)
            end,
            Req1;
        false ->
            Req1
    end.

publish_gauge(Key, Val) ->
    case folsom_metrics:notify({Key, Val}) of
        {error, _, nonexistent_metric} ->
            folsom_metrics:new_gauge(Key),
            folsom_metrics:notify({Key, Val});
        ok ->
            ok
    end.

publish_counter(Key, Val) ->
    KeyBin = phoenix_util:convert_to_binary(Key),
    case folsom_metrics:notify({KeyBin, {inc, Val}}) of
        {error, _, nonexistent_metric} ->
            folsom_metrics:new_counter(Key),
            folsom_metrics:notify({Key, {inc, Val}});
        ok ->
            ok
    end.

publish_histogram(Key, Val) ->
    case folsom_metrics:notify({Key, Val}) of
        {error, _, nonexistent_metric} ->
            folsom_metrics:new_histogram(Key),
            folsom_metrics:notify({Key, Val});
        ok ->
            ok
    end.

format_ip({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".",
     integer_to_list(C), ".", integer_to_list(D)].


http_status_to_metric_name(Status) when Status < 200 -> <<"1xx">>;
http_status_to_metric_name(Status) when Status < 300 -> <<"2xx">>;
http_status_to_metric_name(Status) when Status < 400 -> <<"3xx">>;
http_status_to_metric_name(Status) when Status < 500 -> <<"4xx">>;
http_status_to_metric_name(Status) when Status < 600 -> <<"5xx">>;
http_status_to_metric_name(_) -> <<"bad_status">>.

%% API to publish metrics for http error codes for a given vendor
%% Call this function to log error code level metrics for vendor API calls
%% Error response should be {ok, {{_, ErrorCodeAsInteger, _}, _, _}}
publish_http_error_code_metrics(ErrorResponse, VendorName) ->
    ErrorCode =
        case ErrorResponse of
            {ok, {{_, ErrorCodeAsInteger, _}, _, _}} ->
                integer_to_list(ErrorCodeAsInteger);
            _ ->
                ?UNKNOWN_ERROR_CODE
        end,
    Metric = get_error_code_metric_key(VendorName, ErrorCode),
    publish_counter(Metric, ?ONE).

get_error_code_metric_key(VendorName, ErrorCode) ->
    ?METRIC_BASE_NAME ++ ?DELIMITER ++ VendorName ++ ?DELIMITER ++ ?SERVICE_LISTING ++ ?DELIMITER
        ++ ?RESULT_ERROR_CODE ++ ErrorCode.
