-module(phoenix_cache_util).
-author("romilchoudhary").

-include("phoenix_constants.hrl").

%% API
-export([available_caches/0]).
-export([cache_start_link/2]).
-export([put/3, get/2, remove/2]).
-export([async_put/3, async_remove/2]).

-export([put/2, get/1, remove/1]).
-export([async_put/2, async_remove/1]).


available_caches() ->
    Caches = application:get_env(?APPLICATION_NAME, caches, []),
    lists:foldl(fun({CacheName, CacheOptions}, AccIn) ->
        PropList = [{
            convert_to_binary(X), convert_to_binary(Y)} ||
            {X, Y} <- CacheOptions],
        M = maps:from_list(PropList),
        M2 = M#{<<"_name">> => convert_to_binary(CacheName)},
        [M2 | AccIn]
                end, [], Caches).

cache_start_link(CacheOptions, CacheName) ->
    case proplists:get_value(enable, CacheOptions, false) of
        true ->
            %% drops cache if it exists already
            %% cache:drop(?CACHE_GENERIC),
            MemoryBytes = proplists:get_value(
                memory_bytes, CacheOptions,
                ?DEFAULT_CACHE_MEMORY_BYTES),
            Segments = proplists:get_value(
                segments, CacheOptions, ?DEFAULT_CACHE_SEGMENTS),
            TtlSec = proplists:get_value(
                ttl_sec, CacheOptions, ?DEFAULT_CACHE_TTL_SEC),
            {ok, _} = cache:start_link(CacheName,
                [
                    {memory, MemoryBytes},
                    {n, Segments},
                    {ttl, TtlSec}
                ]);
        _ ->
            ok
    end.

get(Key, CacheName) ->
    Caches = application:get_env(?APPLICATION_NAME, caches, []),
    CacheOptions = proplists:get_value(CacheName, Caches, []),
    case proplists:get_value(enable, CacheOptions, false) of
        true ->
            %% lager:debug("Get ~p from local", [Key]),
            Value = cache:get(CacheName, Key),
            case Value of
                undefined -> {error, not_found};
                Data -> {ok, Data}
            end;
        false ->
            {error, not_found}
    end.

put(Key, Data, CacheName) ->
    Caches = application:get_env(?APPLICATION_NAME, caches, []),
    CacheOptions = proplists:get_value(CacheName, Caches, []),
    case proplists:get_value(enable, CacheOptions, false) of
        true ->
            cache:put(CacheName, Key, Data);
        false ->
            ok
    end.

async_put(Key, Data, CacheName) ->
    Caches = application:get_env(?APPLICATION_NAME, caches, []),
    CacheOptions = proplists:get_value(CacheName, Caches, []),
    case proplists:get_value(enable, CacheOptions, false) of
        true ->
            cache:put_(CacheName, Key, Data);
        false ->
            ok
    end.

remove(Key, CacheName) ->
    Caches = application:get_env(?APPLICATION_NAME, caches, []),
    CacheOptions = proplists:get_value(CacheName, Caches, []),
    case proplists:get_value(enable, CacheOptions, false) of
        true ->
            cache:remove(CacheName, Key);
        false ->
            ok
    end.

async_remove(Key, CacheName) ->
    Caches = application:get_env(?APPLICATION_NAME, caches, []),
    CacheOptions = proplists:get_value(CacheName, Caches, []),
    case proplists:get_value(enable, CacheOptions, false) of
        true ->
            cache:remove_(CacheName, Key);
        false ->
            ok
    end.

get(Key) ->
    get(Key, ?CACHE_GENERIC).

put(Key, Data) ->
    put(Key, Data, ?CACHE_GENERIC).

async_put(Key, Data) ->
    async_put(Key, Data, ?CACHE_GENERIC).

remove(Key) ->
    remove(Key, ?CACHE_GENERIC).

async_remove(Key) ->
    async_remove(Key, ?CACHE_GENERIC).

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_to_binary(X) when is_binary(X) ->
    X;
convert_to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
convert_to_binary(X) when is_integer(X) ->
    integer_to_binary(X);
convert_to_binary(X) when is_list(X) ->
    phoenix_util:convert_to_utf8_binary(X).
