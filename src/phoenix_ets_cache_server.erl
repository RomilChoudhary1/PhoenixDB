-module(phoenix_ets_cache_server).
-author("hiteshv").

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3, put/3]).

-define(SERVER, ?MODULE).

-record(state, {
    name   = undefined         :: atom()    %% name of cache bucket
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(Opts) ->
    gen_server:start_link(?MODULE, [undefined, Opts], []).

start_link({global, Name}, Opts) ->
    gen_server:start_link({global, Name}, ?MODULE, [Name, Opts], []);
start_link(Name, Opts) ->
    gen_server:start_link({local, Name},  ?MODULE, [Name, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Name, _Opts]) ->
    lager:info("gen-server-started ~p", [Name]),
    {ok, #state{name=Name}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({put, Key, Val}, #state{name = TableName} = State) ->
    ets:insert(TableName, {Key, Val}),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

put(Cache, Key, Val) ->
    send(Cache, {put, Key, Val}).

%% asynchronous call to server,
%% the request is acknowledged using reference
%%cast(Pid, Req) ->
%%    Ref = erlang:make_ref(),
%%    erlang:send(Pid, {'$gen_call', {self(), Ref}, Req}, [noconnect]),
%%    Ref.

%%
%% fire-and-forget
send(Pid, Req) ->
    gen_server:cast(Pid, Req).