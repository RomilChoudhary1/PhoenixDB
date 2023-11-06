-module(phoenix_ets_store).
-author("hiteshv").
-include("phoenix_constants.hrl").


%% API
-export([create_ets_table/1,
    put/3,
    async_put/3,
    get/2,
    delete/2,
    get_all_ets_table/0]).


create_ets_table(TableName) when is_atom(TableName) ->
    case ets:info(TableName) of
        undefined ->
            lager:info("Table_Created ~p", [TableName]),
            ets:new(TableName,
                [
                    set,
                    named_table,
                    public,
                    protected,
                    {read_concurrency, true},
                    {write_concurrency, true}
                ]),
            {ok, _} = phoenix_ets_cache_server:start_link(TableName,
                [
                    {table_name, TableName}
                ]);
        TableInfo ->
            proplists:get_value(id, TableInfo)
    end.

async_put(TableName, K, V) when is_atom(TableName) ->
    phoenix_ets_cache_server:put(TableName, K, V).

put(TableName, K, V) when is_atom(TableName) ->
    ets:insert(TableName, {K, V}).


get(TableName, K) ->
    case ets:lookup(TableName, K) of
        [{_, Val}] ->
            {ok, Val};
        _ ->
            {error, not_found}
    end.

delete(TableName, K) ->
    ets:delete(TableName, K).

get_all_ets_table() ->
    case application:get_env(?APPLICATION_NAME, ets_tables) of
        undefined ->
            [{cache_generic, []}];
        {ok, Value} -> Value
    end.