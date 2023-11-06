-module(phoenix_util).
-author("romilchoudhary").

%% API
-export([node_uptime/1
    , convert_to_list/1
    , convert_to_integer/1
    , convert_to_binary/1
    , subtract_hour_min_to_datetime/4
    , get_utc_seconds/0, convert_to_utf8_binary/1]).

%% @doc get node uptime in millisecond or second.
node_uptime(millisecond) ->
    {TotalTimeMsec, _TimeSinceLastCallMsec} = erlang:statistics(wall_clock),
    TotalTimeMsec;
node_uptime(second) ->
    node_uptime(millisecond) div 1000.

%% @doc Convert types to list.
convert_to_list(V) when is_list(V) ->
    V;
convert_to_list(V) when is_binary(V) ->
    binary_to_list(V);
convert_to_list(V) when is_atom(V) ->
    atom_to_list(V);
convert_to_list(V) when is_integer(V) ->
    integer_to_list(V);
convert_to_list(V) when is_float(V) ->
    float_to_list(V);
convert_to_list(V) when is_map(V) ->
    [V].

%%--------------------------------------------------------------------
%% @doc
%% Convert a given input to an integer in case it is not one
%% already. Only binary() and list are supported at present.
-spec convert_to_integer(X :: integer() | binary() | list()) -> integer().
convert_to_integer(X) when is_integer(X) ->
    X;
convert_to_integer(X) when is_binary(X) ->
    binary_to_integer(X);
convert_to_integer(X) when is_list(X) ->
    list_to_integer(X).


convert_to_binary(V) when is_binary(V) ->
    V;
%convert_to_binary(V) when is_list(V) ->
%   list_to_binary(V);
convert_to_binary(V) when is_list(V) ->
    unicode:characters_to_binary(V);
convert_to_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
convert_to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
convert_to_binary(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 2}]).

subtract_hour_min_to_datetime(Date, Hour, Min, Sec) ->
    DateInSecs = calendar:datetime_to_gregorian_seconds(Date),
    HMSInSecs = (Hour * 60 + Min) * 60 + Sec,
    Total = DateInSecs - HMSInSecs,
    calendar:gregorian_seconds_to_datetime(Total).

%%--------------------------------------------------------------------
%% @doc Get current unixtime (time since epoch) as seconds.
%% @todo look at time-wrap
%% http://erlang.org/doc/apps/erts/time_correction.html#Time_Warp
-spec get_utc_seconds() -> pos_integer().
get_utc_seconds() ->
    erlang:system_time(second).

-spec convert_to_utf8_binary(L :: list()) -> binary().
convert_to_utf8_binary(L) ->
    unicode:characters_to_binary(L).