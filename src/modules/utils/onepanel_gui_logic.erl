%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains logic functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================
-module(onepanel_gui_logic).

-include("registered_names.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_release/0]).
-export([login/2, create_user/3, change_password/4]).
-export([initialize_cluster/0, join_cluster/1, get_hosts/1, configure/4,
    get_progress/1]).
-export([get_provider_details/0, get_provider_id/0, get_provider_name/0,
    register_provider/1, unregister_provider/0, modify_details/1]).
-export([get_storages/0, add_storage/1]).
-export([get_spaces/0, get_space_details/1, support_space/1,
    revoke_space_support/1, get_providers/1, get_provider_details/2, get_users/1,
    get_user_details/2]).

%% ====================================================================
%% API functions
%% ====================================================================

get_release() ->
    case application:get_env(onepanel, release) of
        {ok, Release} -> Release;
        undefined -> case application:get_env(?APP_NAME, release) of
            {ok, Release} -> Release;
            undefined -> undefined
        end
    end.

login(Username, Password) ->
    Headers = [onepanel_gui_rest:get_basic_auth_header(Username, Password)],
    case onepanel_gui_rest:noauth_request(
        <<"/users/", Username/binary>>, get, Headers, <<>>
    ) of
        {ok, 200, _, Body} ->
            case lists:keyfind(<<"userRole">>, 1, Body) of
                {_, <<"admin">>} -> ok;
                _ -> {error, ?AUTHORIZATION_ERROR}
            end;
        {ok, _, _, _} -> {error, ?AUTHENTICATION_ERROR};
        {error, Reason} ->
            ?error("User login failed due to: ~p", [Reason]),
            {error, ?INTERNAL_SERVER_ERROR}
    end.


create_user(Username, Password, Role) ->
    Body = [{username, Username}, {password, Password}, {userRole, Role}],
    case onepanel_gui_rest:request(<<"/users">>, post, Body) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


change_password(Username, CurrentPassword, NewPassword, NewPassword) ->
    Headers = [onepanel_gui_rest:get_basic_auth_header(Username, CurrentPassword)],
    Body = [{password, NewPassword}],
    case onepanel_gui_rest:noauth_request(<<"/users/", Username/binary>>, patch, Headers, Body) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end;

change_password(_, _, _, _) ->
    {error, <<"Passwords do not match.">>}.


initialize_cluster() ->
    Body = [{cookie, erlang:get_cookie()}],
    case onepanel_gui_rest:request(<<"/hosts">>, put, Body) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


join_cluster(Hosts) ->
    ClusterHost = erlang:list_to_binary(os:cmd("hostname -f") -- "\n"),
    Body = [{cookie, erlang:get_cookie()}],
    lists:foreach(fun(Host) ->
        onepanel_gui_rest:noauth_request(<<"https://", Host/binary, ":9443">>,
            <<"/hosts?clusterHost=", ClusterHost/binary>>, put, [], Body, [insecure])
    end, Hosts).


get_hosts(cluster) ->
    case onepanel_gui_rest:request(<<"/hosts">>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end;

get_hosts(discovered) ->
    case onepanel_gui_rest:request(<<"/hosts?discovered=true">>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end;

get_hosts(Type) ->
    Prefix = onepanel_gui_rest:get_prefix(),
    Endpoint = <<Prefix/binary, "/", (erlang:atom_to_binary(Type, utf8))/binary>>,
    case onepanel_gui_rest:request(Endpoint, get) of
        {ok, 200, _, Body} ->
            {Hosts, _} = lists:unzip(Body),
            {ok, Hosts};
        {ok, 404, _, _} -> {error, not_found};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


configure(MainCmHost, CmHosts, WrHosts, DbHosts) ->
    {ok, ClusterHosts} = get_hosts(cluster),
    Hosts = lists:usort(CmHosts ++ WrHosts ++ DbHosts),
    Prefix = onepanel_gui_rest:get_prefix(),
    Body = [
        {cluster, [
            {domainName, get_domain(hd(Hosts))},
            {nodes, lists:map(fun(Host) ->
                {Host, [{hostname, get_hostname(Host)}]}
            end, Hosts)},
            {managers, [
                {mainNode, MainCmHost},
                {nodes, CmHosts}
            ]},
            {workers, [
                {nodes, WrHosts}
            ]},
            {databases, [
                {nodes, DbHosts}
            ]}
        ]}
    ],
    join_cluster(Hosts -- ClusterHosts),
    case onepanel_gui_rest:request(<<Prefix/binary, "/configuration">>, put, Body) of
        {ok, 201, Headers, _} ->
            {ok, proplists:get_value(<<"location">>, Headers)};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_progress(Endpoint) ->
    case onepanel_gui_rest:request(Endpoint, get) of
        {ok, 200, _, Body} ->
            case proplists:get_value(<<"status">>, Body) of
                <<"ok">> -> ok;
                <<"running">> -> case proplists:get_value(<<"steps">>, Body) of
                    [] -> running;
                    Steps -> {running, hd(lists:reverse(Steps))}
                end;
                <<"error">> ->
                    ?error("Progress error: ~p", [Body]),
                    {error, proplists:get_value(<<"description">>, Body)}
            end;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_provider_details() ->
    case onepanel_gui_rest:request(<<"/provider">>, get) of
        {ok, 200, _, Body} -> {ok, maps:from_list(Body)};
        {ok, 404, _, _} -> {ok, #{}};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_provider_id() ->
    case get_provider_details() of
        {ok, #{<<"id">> := Id}} -> Id;
        _ -> undefined
    end.


get_provider_name() ->
    case get_provider_details() of
        {ok, #{<<"name">> := Name}} -> Name;
        _ -> <<"onepanel">>
    end.


register_provider(Body) ->
    case onepanel_gui_rest:request(<<"/provider">>, put, Body) of
        {ok, 201, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


unregister_provider() ->
    case onepanel_gui_rest:request(<<"/provider">>, delete) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


modify_details(Body) ->
    case onepanel_gui_rest:request(<<"/provider">>, patch, Body) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_storages() ->
    case onepanel_gui_rest:request(<<"/provider/storages">>, get) of
        {ok, 200, _, Body} ->
            {ok, lists:map(fun({Name, Params}) ->
                {Name, maps:from_list(Params)}
            end, Body)};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


add_storage(Body) ->
    case onepanel_gui_rest:request(<<"/provider/storages">>, put, Body) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_spaces() ->
    case onepanel_gui_rest:request(<<"/provider/spaces">>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_space_details(SpaceId) ->
    case onepanel_gui_rest:request(<<"/provider/spaces/", SpaceId/binary>>, get) of
        {ok, 200, _, Body} -> {ok, maps:from_list(Body)};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


support_space(Body) ->
    case onepanel_gui_rest:request(<<"/provider/spaces">>, put, Body) of
        {ok, 200, _, Body2} -> {ok, proplists:get_value(<<"id">>, Body2)};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


revoke_space_support(SpaceId) ->
    case onepanel_gui_rest:request(<<"/provider/spaces/", SpaceId/binary>>, delete) of
        {ok, 204, _, _} -> ok;
        Other -> onepanel_gui_rest:translate_error(Other)
    end.

get_providers(SpaceId) ->
    case onepanel_gui_rest:request(<<"/provider/spaces/", SpaceId/binary, "/providers">>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_provider_details(SpaceId, ProviderId) ->
    case onepanel_gui_rest:request(<<"/provider/spaces/", SpaceId/binary, "/providers/", ProviderId/binary>>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_users(SpaceId) ->
    case onepanel_gui_rest:request(<<"/provider/spaces/", SpaceId/binary, "/users">>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.


get_user_details(SpaceId, UserId) ->
    case onepanel_gui_rest:request(<<"/provider/spaces/", SpaceId/binary, "/users/", UserId/binary>>, get) of
        {ok, 200, _, Body} -> {ok, Body};
        Other -> onepanel_gui_rest:translate_error(Other)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


get_hostname(Host) ->
    [Hostname | _] = binary:split(Host, <<".">>, [global]),
    Hostname.


get_domain(Host) ->
    [_, Token | Tokens] = binary:split(Host, <<".">>, [global]),
    lists:foldl(fun(T, Acc) ->
        <<Acc/binary, ".", T/binary>>
    end, Token, Tokens).
