%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains REST client functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================
-module(onepanel_gui_rest).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_basic_auth_header/2, get_prefix/0, translate_error/1]).
-export([noauth_request/2, noauth_request/3, noauth_request/4, noauth_request/5,
    noauth_request/6]).
-export([request/2, request/3, request/4, request/5]).

%% ====================================================================
%% API functions
%% ====================================================================

get_basic_auth_header(Username, Password) ->
    Hash = base64:encode(<<Username/binary, ":", Password/binary>>),
    {<<"authorization">>, <<"Basic ", Hash/binary>>}.


get_prefix() ->
    case onepanel_gui_logic:get_release() of
        onezone -> <<"/zone">>;
        oneprovider -> <<"/provider">>
    end.


translate_error({error, Reason} = E) ->
    ?error("REST error: ~p", [E]),
    {error, Reason};

translate_error({ok, _, _, Body} = E) ->
    ?error("REST error: ~p", [E]),
    case proplists:get_value(<<"description">>, Body) of
        undefined -> {error, <<"Server encountered unexpected error">>};
        Description -> {error, Description}
    end.


noauth_request(Endpoint, Method) ->
    noauth_request(Endpoint, Method, <<>>).

noauth_request(Endpoint, Method, Body) ->
    noauth_request(Endpoint, Method, [], Body).

noauth_request(Endpoint, Method, Headers, Body) ->
    noauth_request(Endpoint, Method, Headers, Body, []).

noauth_request(Endpoint, Method, Headers, Body, Options) ->
    {ok, Url} = application:get_env(?APP_NAME, onepanel_server_url),
    noauth_request(Url, Endpoint, Method, Headers, Body, Options).

noauth_request(Url, Endpoint, Method, Headers, Body, Options) ->
    {ok, Prefix} = application:get_env(?APP_NAME, onepanel_server_rest_prefix),
    NewHeaders = [{"content-type", "application/json"} | Headers],
    case http_client:request(Method, <<Url/binary, Prefix/binary, Endpoint/binary>>,
        NewHeaders, json_utils:encode(Body), [insecure | Options]
    ) of
        {ok, Code, RespHeaders, RespBody} ->
            {ok, Code, RespHeaders, json_utils:decode(RespBody)};
        {error, Reason} ->
            {error, Reason}
    end.


request(Endpoint, Method) ->
    request(Endpoint, Method, <<>>).

request(Endpoint, Method, Body) ->
    request(Endpoint, Method, [], Body).

request(Endpoint, Method, Headers, Body) ->
    request(Endpoint, Method, Headers, Body, []).

request(Endpoint, Method, Headers, Body, Options) ->
    NewHeaders = case ets:lookup(store, user_auth) of
        [{user_auth, {Id, Auth}} | _] ->
            [get_basic_auth_header(Id, base64:decode(Auth)) | Headers];
        _ ->
            Headers
    end,
    noauth_request(Endpoint, Method, NewHeaders, Body, Options).
