-module(phoenix_folsom_metric_sup).

-behaviour(supervisor).

-include("phoenix_constants.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Name, Index, Type, SupPid),
        {Name, {I, start_link, [Index, SupPid]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(SupPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SupPid]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([SupPid]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, FolsomDdbConfig} = application:get_env(?APPLICATION_NAME,
                                                folsom_graphite),
    FolsomDdbBuckets = proplists:get_value(buckets, FolsomDdbConfig, []),
    NumWorkers = length(FolsomDdbBuckets),
    Specs1 = [?CHILD(phoenix_folsom_graphite_server,
        proplists:get_value(name,
                            lists:nth(Index, FolsomDdbBuckets)),
        Index, worker, SupPid) || Index <- lists:seq(1, NumWorkers)],
    {ok, { SupFlags, Specs1} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
