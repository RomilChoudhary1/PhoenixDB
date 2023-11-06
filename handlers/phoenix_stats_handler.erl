-module(phoenix_stats_handler).
-author("romilchoudhary").

-include("phoenix_constants.hrl").

-export([init/2]).

-define(LAGER_ATTRS, [{type, handler}]).

init(Req0, Opts) ->
    lager:debug(?LAGER_ATTRS, "[~p] ~p received request = ~p, Opts = ~p",
        [self(), ?MODULE, Req0, Opts]),
    %% #{peer := {IP, Port}} = Req0,
    %% IpBinary = list_to_binary(inet:ntoa(IP)),
    Response = cluster_util:node_info(),
%%    Nodes = [node() | nodes()],
%%    {Responses, _BadNodes} = rpc:multicall(
%%        Nodes, cluster_util, node_info, [],
%%        ?DEFAULT_CLUSTER_CALL_TIMEOUT_MSEC),
    Content = jiffy:encode(Response),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json; charset=utf-8">>,
        <<"connection">> => <<"keep-alive">>}, Content, Req0),
    {ok, Req, Opts}.

