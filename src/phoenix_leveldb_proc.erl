-module(phoenix_leveldb_proc).
-author("romilchoudhary").

-behaviour(gen_server).

-include("phoenix_constants.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/1, timer_expired/1, handle_info/2]).

-define(PATH_PREFIX, "/opt/phoenix_datastore/leveldb-data/").
-define(EXPIRY_TIMER_SECS, 6 * 60 * 60* 1000).
-define(LEVELDB_OPERATION_TIMEOUT, 120 * 1000).


start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(_Opts) ->
    process_flag(priority, high),
    {ok, #{}}.

handle_call({create, ShardKey, Id, Value}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = create(Id, Value, Ref, ShardKey),
    {reply, Response, State};
handle_call({read, ShardKey, Id}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = read(Id, Ref, ShardKey),
    {reply, Response, State};
handle_call({delete, ShardKey, Id}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = delete(Id, Ref, ShardKey),
    {reply, Response, State};
handle_call({get_all_expired_ids, ShardKey}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = get_all_expired_ids(Ref, ShardKey),
    {reply, Response, State};
handle_call({get_all_ids, ShardKey}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = get_all_ids(Ref, ShardKey),
    {reply, Response, State};
handle_call({delete_ids, ShardKey, Ids}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = delete_ids(Ids, Ref, ShardKey),
    {reply, Response, State};
handle_call({delete_expired_ids, ShardKey}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = delete_expired_ids(Ref, ShardKey),
    {reply, Response, State};
handle_call({delete_all_ids, ShardKey}, _From, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    Response = delete_all_ids(Ref, ShardKey),
    {reply, Response, State};
handle_call({drop_shard, ShardKey}, _From, State) ->
    Path = ?PATH_PREFIX ++ ShardKey,
    {Ref, TRef} = get_shard_ref(ShardKey),
    application:unset_env(?APPLICATION_NAME, ShardKey),
    Response = drop_shard(Path, Ref),
    timer:cancel(TRef),
    {reply, Response, State};
handle_call(_Default, _From, State) ->
    {reply, undefined, State}.

handle_cast({create, ShardKey, Id, Value}, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    create(Id, Value, Ref, ShardKey),
    {noreply, State};
handle_cast({delete, ShardKey, Id}, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    delete(Id, Ref, ShardKey),
    {noreply, State};
handle_cast({delete_ids, ShardKey, Ids}, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    delete_ids(Ids, Ref, ShardKey),
    {noreply, State};
handle_cast({delete_all_ids, ShardKey}, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    delete_all_ids(Ref, ShardKey),
    {noreply, State};
handle_cast({delete_expired_ids, ShardKey}, State) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    delete_expired_ids(Ref, ShardKey),
    {noreply, State};
handle_cast({drop_shard, ShardKey}, State) ->
    Path = ?PATH_PREFIX ++ ShardKey,
    {Ref, TRef} = get_shard_ref(ShardKey),
    application:unset_env(?APPLICATION_NAME, Path),
    drop_shard(Path, Ref),
    timer:cancel(TRef),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

get_connections_server_pid() ->
    try
        case palma:pid(leveldb_connections_pool) of
            Pid when is_pid(Pid) ->
                {ok, Pid};
            _ ->
                {error, disconnected}
        end
    catch
        _:_ ->
            {error, disconnected}
    end.

get_shard_ref(ShardKey) ->
    case application:get_env(?APPLICATION_NAME, ShardKey) of
        undefined ->
            {ok, Pid}  = get_connections_server_pid(),
            gen_server:call(Pid, {get_connection, ShardKey}, ?LEVELDB_OPERATION_TIMEOUT);
        {ok, {Ref, TRef}} ->
            {Ref, TRef}
    end.

create(Id, Value, Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        case eleveldb:put(Ref, Id, Value, []) of
            ok ->
                cache_if_required(Id, Value),
                log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
                ok;
            {error, _Reason} ->
                log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
                error
        end
    catch _C:_E ->
        log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
        error
    end.

read(Id, Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    case phoenix_ets_store:get(?CACHE_GENERIC, Id) of
        {ok, Data} ->
            {ok, Data};
        _ ->
            case eleveldb:get(Ref, Id, []) of
                {ok, V} ->
                    cache_if_required(Id, V),
                    log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
                    {ok, V};
                not_found ->
                    log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
                    {error, not_found};
                {error, Reason} ->
                    log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
                    {error, Reason}
            end
    end.

delete(Id, Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        eleveldb:delete(Ref, Id, []),
        case Id of
            <<0, _K/binary>> ->
                log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
                {ok, success};
            _ ->
                phoenix_ets_store:delete(?CACHE_GENERIC, Id),
                log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
                {ok, success}
        end
    catch _C:_E ->
        log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
        {error, undefined}
    end.


get_all_expired_ids(Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        Resp = eleveldb:fold_keys(Ref,
            fun(K, AccIn) ->
                {Mega, Sec, _Micro} = os:timestamp(),
                NowInSeconds = Mega * 1000000 + Sec,
                case K of
                    <<0, 0, 0, 0, ExpiryEpoch:64, Id/binary>> ->
                        case ExpiryEpoch < NowInSeconds of
                            true ->
                                [Id, K] ++ AccIn;
                            false ->
                                throw({ok, AccIn})
                        end;
                    _ ->
                        throw({ok, AccIn})
                end
            end, [], [{fill_cache, false}]),
        {ok, lists:reverse(Resp)}
    catch _C:E ->
        case E of
            {ok, Response} ->
                log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
                {ok, lists:reverse(Response)};
            _ ->
                log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
                E
        end
    end.

get_all_ids(Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        Resp = eleveldb:fold_keys(Ref,
            fun(K, AccIn) ->
                [K] ++ AccIn
            end, [], [{fill_cache, false}]),
        log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
        {ok, lists:reverse(Resp)}
    catch _C:_E ->
        log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
        {error, failed}
    end.

delete_ids(Ids, Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        lists:foldl(fun(Id, _AccIn) ->
            delete(Id, Ref, ShardKey) end, [], Ids),
        log_utils:publish_counter(<<"bm.event.ld.success.", BinShardKey/binary>>, 1),
        ok
    catch _C:_E ->
        log_utils:publish_counter(<<"bm.event.ld.fail.", BinShardKey/binary>>, 1),
        error
    end.

delete_expired_ids(Ref, ShardKey) ->
    case get_all_expired_ids(Ref, ShardKey) of
        {ok, ExpiredIds} ->
            delete_ids(ExpiredIds, Ref, ShardKey);
        Exp ->
            Exp
    end.

delete_all_ids(Ref, ShardKey) ->
    case get_all_ids(Ref, ShardKey) of
        {ok, Ids} ->
            delete_ids(Ids, Ref, ShardKey);
        Exp ->
            Exp
    end.

timer_expired(ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        case palma:pid(leveldb_pool) of
            Pid when is_pid(Pid) ->
                gen_server:cast(Pid, {delete_expired_ids, ShardKey});
            _ ->
                log_utils:publish_counter(<<"bm.event.lde.fail.", BinShardKey/binary>>, 1),
                {error, disconnected}
        end
    catch
        _:_ ->
            log_utils:publish_counter(<<"bm.event.lde.fail.", BinShardKey/binary>>, 1),
            {error, disconnected}
    end.

drop_shard(Path, Ref) ->
    eleveldb:close(Ref),
    eleveldb:destroy(Path, []),
    file:del_dir(Path).

cache_if_required(K, V) ->
    case is_normal_key(K) of
        true ->
            phoenix_ets_store:async_put(?CACHE_GENERIC, K,V);
        _ ->
            ok
    end.

is_normal_key(Key) ->
    case Key of
        <<0, _Id/binary>> -> false;
        _ -> true
    end.
