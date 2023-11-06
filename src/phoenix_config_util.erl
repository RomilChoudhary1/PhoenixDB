-module(phoenix_config_util).
-author("romilchoudhary").

%% API
-export([generic_http_client_set_options/0
    , is_mnesia_enabled/0
    , get_org_unit/0]).

-include("phoenix_constants.hrl").

generic_http_client_config() ->
    application:get_env(?APPLICATION_NAME, generic_http_client, []).

generic_http_client_set_options() ->
    HttpClientConfig = generic_http_client_config(),
    proplists:get_value(set_options,
        HttpClientConfig,
        ?DEFAULT_HTTP_CLIENT_SET_OPTIONS).

is_mnesia_enabled() ->
    application:get_env(
        ?APPLICATION_NAME, mnesia_enabled,
        false).

-spec get_org_unit() -> binary().
get_org_unit() ->
    integer_to_binary(get_int_org_unit()).

-spec get_int_org_unit() -> non_neg_integer().
get_int_org_unit() ->
    application:get_env(
        ?APPLICATION_NAME, org_unit,
        ?DEFAULT_ORG_UNIT).
