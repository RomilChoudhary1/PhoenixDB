-module(phoenix_connections_server).
-author("romilchoudhary").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PATH_PREFIX, "/opt/phoenix_datastore/leveldb-data/").
-define(EXPIRY_TIMER_SECS, 6 * 60 * 60 * 1000).
-define(LEVELDB_OPERATION_TIMEOUT, 120 * 1000).
-define(TIMEOUT_MSEC, 60 * 60 * 1000).


-include("phoenix_constants.hrl").

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(_Opts) ->
    {ok, #{}}.

handle_call({get_connection, ShardKey}, _From, State) ->
    Path = ?PATH_PREFIX ++ ShardKey,
    {Ref, TRef} = get_connection(Path, ShardKey, State),
    {reply, {Ref, TRef}, State};
handle_call(_Msg, _From, State) ->
    {reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_connection(Path, ShardKey, _State) ->
    DbOptions = [{compression, lz4}
        , {write_buffer_size, 167772160}  %% 160*1024*1024 =167772160
        , {use_bloomfilter, true}
    ],
    AppResponse = case application:get_env(?APPLICATION_NAME, ShardKey) of
        undefined ->
            Ref = connect(Path, DbOptions),
            Pid = palma:pid(phoenix_cleanup_pool),
            TRef = erlang:start_timer(?TIMEOUT_MSEC, Pid, {tick, ShardKey}),
            application:set_env(?APPLICATION_NAME, ShardKey, {Ref, TRef}),
            {Ref, TRef};
        {ok, {Ref, TRef}} ->
            {Ref, TRef}
    end,
    AppResponse.

connect(Path, DbOptions) ->
    {ok, Ref} = eleveldb:open(Path, [{create_if_missing, true}] ++ DbOptions),
    Ref.
