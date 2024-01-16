-module(phoenix_cleanup_server).
-author("romilchoudhary").

-behavior(gen_server).

-include("phoenix_constants.hrl").

-define(TIMEOUT_MSEC, 60 * 60 * 1000).
-define(LEVELDB_OPERATION_TIMEOUT, 120 * 1000).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/1, handle_info/2, terminate/2, code_change/3]).


start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(_Opts) ->
    {ok, #{}}.

handle_call(_Msg, _From, State) ->
    {reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _R, {tick, ShardKey}}, State) ->
    Ref = process_tick(ShardKey),
    TRef = erlang:start_timer(?TIMEOUT_MSEC, self(), {tick, ShardKey}),
    application:set_env(?APPLICATION_NAME, ShardKey, {Ref, TRef}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_tick(ShardKey) ->
    {Ref, _TRef} = get_shard_ref(ShardKey),
    delete_expired_ids(Ref, ShardKey),
    Ref.

get_shard_ref(ShardKey) ->
    case application:get_env(?APPLICATION_NAME, ShardKey) of
        {ok, {Ref, TRef}} ->
            {Ref, TRef}
    end.

delete_expired_ids(Ref, ShardKey) ->
    case get_all_expired_ids(Ref, ShardKey) of
        {ok, ExpiredIds} ->
            delete_ids(ExpiredIds, Ref, ShardKey);
        Exp ->
            Exp
    end.

get_all_expired_ids(Ref, ShardKey) ->
    MagicBinary = <<0, 0, 0, 0, 0, 0, 0, 0>>,
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
                                case eleveldb:get(Ref, Id, []) of
                                    {ok, Response} ->
                                        case Response of
                                            <<MagicBinary:8/binary, _Value/binary>> ->
                                                [K] ++ AccIn;
                                            <<NewExpiryEpoch:64, _Val/binary>> ->
                                                DeltaExp = NewExpiryEpoch - NowInSeconds,
                                                case DeltaExp < 0 of
                                                    true ->
                                                        [Id, K] ++ AccIn;
                                                    _ ->
                                                        [K] ++ AccIn
                                                end
                                        end;
                                    _ ->
                                        [K] ++ AccIn
                                end;
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
                log_utils:publish_counter(<<"bm.event.lde.success.", BinShardKey/binary>>, 1),
                {ok, lists:reverse(Response)};
            _ ->
                log_utils:publish_counter(<<"bm.event.lde.fail.", BinShardKey/binary>>, 1),
                E
        end
    end.


delete_ids(Ids, Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        lists:foldl(fun(Id, _AccIn) ->
            delete(Id, Ref, ShardKey) end, [], Ids),
        log_utils:publish_counter(<<"bm.event.lde.success.", BinShardKey/binary>>, 1),
        ok
    catch _C:_E ->
        log_utils:publish_counter(<<"bm.event.lde.fail.", BinShardKey/binary>>, 1),
        error
    end.


delete(Id, Ref, ShardKey) ->
    BinShardKey = phoenix_util:convert_to_binary(ShardKey),
    try
        eleveldb:delete(Ref, Id, []),
        case Id of
            <<0, _K/binary>> ->
                log_utils:publish_counter(<<"bm.event.lde.success.", BinShardKey/binary>>, 1),
                {ok, success};
            _ ->
                CombinedKey = <<BinShardKey/binary, Id/binary>>,
                phoenix_ets_store:delete(?CACHE_GENERIC, CombinedKey),
                log_utils:publish_counter(<<"bm.event.lde.success.", BinShardKey/binary>>, 1),
                {ok, success}
        end
    catch _C:_E ->
        log_utils:publish_counter(<<"bm.event.lde.fail.", BinShardKey/binary>>, 1),
        {error, undefined}
    end.
