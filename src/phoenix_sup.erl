%%%-------------------------------------------------------------------
%% @doc phoenix top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(phoenix_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-include("phoenix_constants.hrl").

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SelfPid = self(),
    SysmonConfig = application:get_env(?APPLICATION_NAME, sysmon, []),
    SysmonSpecs = case proplists:get_value(enabled, SysmonConfig, false) of
                      true ->
                          [{phoenix_sysmon_server,
                              {phoenix_sysmon_server, start_link, []},
                              permanent, 5000, worker, [phoenix_sysmon_server]}];
                      false ->
                          []
                  end,
    FolsomMetricSpecs = [{phoenix_folsom_metric_sup,
        {phoenix_folsom_metric_sup, start_link, [SelfPid]},
        permanent, 5000, supervisor,
        [phoenix_folsom_metric_sup]}],
    DelayedOpts = [],
    DelayedStartupServerSpecs = [{phoenix_delaystart_server, {
        phoenix_delaystart_server,
        start_link, [DelayedOpts]},
        permanent, 5000, worker,
        [phoenix_delaystart_server]}],
    {ok, { {one_for_one, 1000, 3600}, SysmonSpecs ++ FolsomMetricSpecs
            ++ DelayedStartupServerSpecs}}.
