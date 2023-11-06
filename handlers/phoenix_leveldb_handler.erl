-module(phoenix_leveldb_handler).
-author("romilchoudhary").

-include("phoenix_constants.hrl").

-define(LEVELDB_OPERATION_TIMEOUT, 120 * 1000).
-define(LAGER_ATTRS, [{type, handler}]).

-export([init/2]).

init(Req0, State) ->
    lager:debug(?LAGER_ATTRS, "[~p] ~p received request = ~p, State = ~p",
        [self(), ?MODULE, Req0, State]),
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_get_request(Req0, State);
        <<"POST">> ->
            handle_post_request(Req0, State);
        <<"DELETE">> ->
            handle_delete_request(Req0, State);
        _ ->
            Req2 = cowboy_req:reply(?HTTP_CODE_METHOD_NOT_ALLOWED, Req0),
            {ok, Req2, State}
    end.

%%%===================================================================
%%% GET REQUESTS
%%%===================================================================

handle_get_request(Req0, []) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    Id = cowboy_req:binding(id, Req0),
    ShardKey = "default",
    case read_method(ShardKey, Id) of
        {ok, Response} ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(Response, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_get_request(Req0, [get_expired_ids]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = "default",
    case get_expired_ids(ShardKey) of
        {ok, Response} ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(jiffy:encode(Response), Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_get_request(Req0, [get_all_ids]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = "default",
    case get_all_ids(ShardKey) of
        {ok, Response} ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(jiffy:encode(Response), Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        Resp ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(jiffy:encode(Resp), Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_get_request(Req0, [shard_handler]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    Id = cowboy_req:binding(id, Req0),
    case read_method(ShardKey, Id) of
        {ok, Response} ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(Response, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_get_request(Req0, [get_expired_ids_for_shard]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    case get_expired_ids(ShardKey) of
        {ok, Response} ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(jiffy:encode(Response), Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_get_request(Req0, [get_all_ids_for_shard]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    case get_all_ids(ShardKey) of
        {ok, Response} ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(jiffy:encode(Response), Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        Resp ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(Resp, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_get_request(Req0, State) ->
    Req2 = cowboy_req:reply(?HTTP_BAD_REQUEST, Req0),
    {ok, Req2, State}.

%%%===================================================================
%%% POST REQUESTS
%%%===================================================================

handle_post_request(Req0, []) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    Id = cowboy_req:binding(id, Req0),
    ShardKey = "default",
    QueryParam = cowboy_req:parse_qs(Req0),
    Expiry = phoenix_util:convert_to_integer(proplists:get_value(<<"exp">>, QueryParam, 0)),
    {ok, Value, _} = read_request_body(Req0),
    case Value of
        <<>> ->
            Status = ?HTTP_BAD_REQUEST,
            Req = send_response(<<"Error Body is Empty">>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Resp = case Expiry of
                       0 ->
                           create_method(ShardKey, Id, Value);
                       _ -> create_method(ShardKey, Id, Value, Expiry)
                   end,
            case Resp of
                {ok, Response} ->
                    BinResponse = phoenix_util:convert_to_binary(Response),
                    Status = ?HTTP_OK_RESPONSE_CODE,
                    Req = send_response(BinResponse, Status, Req0),
                    log_utils:req_log(Status, [], [], LogReq),
                    {ok, Req, []};
                _ ->
                    Status = ?HTTP_BAD_REQUEST,
                    Req = send_response(<<>>, Status, Req0),
                    log_utils:req_log(Status, [], [], LogReq),
                    {ok, Req, []}
            end
    end;
handle_post_request(Req0, [shard_handler]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    Id = cowboy_req:binding(id, Req0),
    QueryParam = cowboy_req:parse_qs(Req0),
    Expiry = phoenix_util:convert_to_integer(proplists:get_value(<<"exp">>, QueryParam, 0)),
    {ok, Value, _} = read_request_body(Req0),
    case Value of
        <<>> ->
            Status = ?HTTP_BAD_REQUEST,
            Req = send_response(<<"Error Body is Empty">>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Resp = case Expiry of
                       0 ->
                           create_method(ShardKey, Id, Value);
                       _ -> create_method(ShardKey, Id, Value, Expiry)
                   end,
            case Resp of
                {ok, Response} ->
                    BinResponse = phoenix_util:convert_to_list(Response),
                    Status = ?HTTP_OK_RESPONSE_CODE,
                    Req = send_response(BinResponse, Status, Req0),
                    log_utils:req_log(Status, [], [], LogReq),
                    {ok, Req, []};
                _ ->
                    Status = ?HTTP_BAD_REQUEST,
                    Req = send_response(<<>>, Status, Req0),
                    log_utils:req_log(Status, [], [], LogReq),
                    {ok, Req, []}
            end
    end;
handle_post_request(Req0, State) ->
    Req2 = cowboy_req:reply(?HTTP_BAD_REQUEST, Req0),
    {ok, Req2, State}.

%%%===================================================================
%%% DELETE REQUESTS
%%%===================================================================

handle_delete_request(Req0, []) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    Id = cowboy_req:binding(id, Req0),
    ShardKey = "default",
    case delete_method(ShardKey, Id) of
        {ok, Response} ->
            BinResponse = phoenix_util:convert_to_list(Response),
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(BinResponse, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, [delete_expired_ids]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = "default",
    case delete_all_expired_ids(ShardKey) of
        ok ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response([], Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, [delete_all_ids]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = "default",
    case delete_all_ids(ShardKey) of
        ok ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response([], Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, [shard_handler]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    Id = cowboy_req:binding(id, Req0),
    case delete_method(ShardKey, Id) of
        {ok, Response} ->
            BinResponse = phoenix_util:convert_to_binary(Response),
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response(BinResponse, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, [delete_expired_ids_for_shard]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    case delete_all_expired_ids(ShardKey) of
        ok ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response([], Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, [delete_all_ids_for_shard]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    case delete_all_ids(ShardKey) of
        ok ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response([], Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, [drop_shard]) ->
    LogReq = log_utils:create_log_req(Req0, ?MODULE),
    ShardKey = phoenix_util:convert_to_list(cowboy_req:binding(shardKey, Req0)),
    case drop_shard_method(ShardKey) of
        ok ->
            Status = ?HTTP_OK_RESPONSE_CODE,
            Req = send_response([], Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []};
        _ ->
            Status = ?HTTP_NOT_FOUND,
            Req = send_response(<<>>, Status, Req0),
            log_utils:req_log(Status, [], [], LogReq),
            {ok, Req, []}
    end;
handle_delete_request(Req0, State) ->
    Req2 = cowboy_req:reply(?HTTP_BAD_REQUEST, Req0),
    {ok, Req2, State}.

%%%===================================================================
%%% INTERNAL METHODS
%%%===================================================================

create_method(ShardKey, Id, Val, ExpirySeconds) ->
    {Mega, Sec, _Micro} = os:timestamp(),
    NowInSeconds = Mega * 1000000 + Sec,
    ExpiryEpochSeconds = NowInSeconds + ExpirySeconds,
    ValWithExpiry = <<ExpiryEpochSeconds:64, Val/binary>>,
    KeyWithExpiry = <<0, 0, 0, 0, ExpiryEpochSeconds:64, Id/binary>>,
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                case gen_server:call(Pid, {create, ShardKey, Id, ValWithExpiry}, ?LEVELDB_OPERATION_TIMEOUT) of
                    ok ->
                        gen_server:cast(Pid, {create, ShardKey, KeyWithExpiry, <<>>}),
                        {ok, true};
                    Error ->
                        {error, Error}
                end;
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

create_method(ShardKey, Id, Val) ->
    MagicBinary = <<0, 0, 0, 0, 0, 0, 0, 0>>,
    ValWithExpiry = <<MagicBinary/binary, Val/binary>>,
    {Mega, Sec, _Micro} = os:timestamp(),
    NowInSeconds = Mega * 1000000 + Sec,
    KeyWithCurrTime = <<0, 0, 0, NowInSeconds:64, Id/binary>>,
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                case gen_server:call(Pid, {create, ShardKey, Id, ValWithExpiry}, ?LEVELDB_OPERATION_TIMEOUT) of
                    ok ->
                        gen_server:cast(Pid, {create, ShardKey, KeyWithCurrTime, <<>>}),
                        {ok, true};
                    Error ->
                        {error, Error}
                end;
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

read_method(ShardKey, Id) ->
    MagicBinary = <<0, 0, 0, 0, 0, 0, 0, 0>>,
    {Mega, Sec, _Micro} = os:timestamp(),
    NowInSeconds = Mega * 1000000 + Sec,
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                {Status, Response} = gen_server:call(Pid, {read, ShardKey, Id}, ?LEVELDB_OPERATION_TIMEOUT),
                case Status of
                    ok ->
                        case Response of
                            <<MagicBinary:8/binary, Value/binary>> ->
                                {ok, Value};
                            <<ExpiryEpoch:64, Val/binary>> ->
                                DeltaExp = ExpiryEpoch - NowInSeconds,
                                case DeltaExp < 0 of
                                    true ->
                                        gen_server:cast(Pid, {delete, Id}),
                                        {error, undefined};
                                    _ ->
                                        {ok, Val}
                                end
                        end;
                    _ ->
                        {error, undefined}
                end;
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

delete_method(ShardKey, Id) ->
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                gen_server:call(Pid, {delete, ShardKey, Id}, ?LEVELDB_OPERATION_TIMEOUT);
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

get_expired_ids(ShardKey) ->
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                Response = gen_server:call(Pid, {get_all_expired_ids, ShardKey}, ?LEVELDB_OPERATION_TIMEOUT),
                case Response of
                    {ok, List} ->
                        {ok, lists:foldr(fun(Key, Acc) ->
                            case Key of
                                <<0, _Id/binary>> ->
                                    Acc;
                                _ ->
                                    [Key | Acc]
                            end end, [], List)};
                    _ ->
                        Response
                end;
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

get_all_ids(ShardKey) ->
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                Response = gen_server:call(Pid, {get_all_ids, ShardKey}, ?LEVELDB_OPERATION_TIMEOUT),
                case Response of
                    {ok, List} ->
                        {ok, lists:foldr(fun(Key, Acc) ->
                            case Key of
                                <<0, _Id/binary>> ->
                                    Acc;
                                _ ->
                                    [Key | Acc]
                            end end, [], List)};
                    _ ->
                        Response
                end;
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

delete_all_expired_ids(ShardKey) ->
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                gen_server:call(Pid, {delete_expired_ids, ShardKey}, ?LEVELDB_OPERATION_TIMEOUT);
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

delete_all_ids(ShardKey) ->
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                gen_server:call(Pid, {delete_all_ids, ShardKey}, ?LEVELDB_OPERATION_TIMEOUT);
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

drop_shard_method(ShardKey) ->
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                gen_server:call(Pid, {drop_shard, ShardKey}, ?LEVELDB_OPERATION_TIMEOUT);
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

read_request_body(Req) ->
    {HttpMaxReadBytes, HttpMaxReadTimeoutMsec} =
        case application:get_env(?APPLICATION_NAME, http_rest) of
            {ok, HttpRestConfig} ->
                A = proplists:get_value(max_read_length,
                    HttpRestConfig, ?DEFAULT_MAX_HTTP_READ_BYTES),
                B = proplists:get_value(max_read_timeout_msec,
                    HttpRestConfig, ?DEFAULT_MAX_HTTP_READ_TIMEOUT_MSEC),
                {A, B};
            _ -> {?DEFAULT_MAX_HTTP_READ_BYTES,
                ?DEFAULT_MAX_HTTP_READ_TIMEOUT_MSEC}
        end,
    Options = #{length => HttpMaxReadBytes, timeout => HttpMaxReadTimeoutMsec},
    cowboy_req:read_body(Req, Options).

send_response(Content, Status, Req0) ->
    Req = cowboy_req:reply(
        Status,
        ?DEFAULT_HTTP_JSON_RESPONSE_HEADER,
        Content, Req0),
    Req.