%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides mapping of onepanel paths to modules
%% that will render the pages.
%% @end
%% ===================================================================
-module(routes).

-include("modules/common.hrl").
-include_lib("n2o/include/wf.hrl").

-export([init/2, finish/2]).

%% ====================================================================
%% API functions
%% ====================================================================

init(State, Ctx) ->
    Path = wf:path(Ctx#context.req),
    RequestedPage = case Path of
        <<"/ws", Rest/binary>> -> Rest;
        Other -> Other
    end,
    {ok, State, Ctx#context{path = Path, module = route(RequestedPage)}}.


finish(State, Ctx) -> {ok, State, Ctx}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% Root page
route(?PAGE_ROOT) ->
    case onepanel_gui_logic:get_release() of
        onezone -> page_installation;
        oneprovider ->
            case onepanel_gui_logic:get_hosts(workers) of
                {error, not_found} -> page_installation;
                _ -> case onepanel_gui_logic:get_provider_id() of
                    undefined -> page_spaces_account;
                    _ -> page_spaces_management
                end
            end
    end;

%% Management pages
route(?PAGE_LOGIN) -> page_login;
route(?PAGE_LOGIN_VALIDATION) -> page_login_validation;
route(?PAGE_LOGOUT) -> page_logout;
route(?PAGE_ABOUT) -> page_about;
route(?PAGE_ERROR) -> page_error;
route(?PAGE_ACCOUNT_SETTINGS) -> page_account_settings;
route(?PAGE_PRIVACY_POLICY) -> page_privacy_policy;

%% Installation pages
route(?PAGE_INSTALLATION) -> page_installation;
route(?PAGE_HOST_SELECTION) -> page_hosts_selection;
route(?PAGE_PRIMARY_CM_SELECTION) -> page_primary_cm_selection;
route(?PAGE_APP_PORTS_CHECK) -> page_app_ports_check;
route(?PAGE_SYSTEM_LIMITS) -> page_system_limits;
route(?PAGE_STORAGE) -> page_storage;
route(?PAGE_INSTALLATION_SUMMARY) -> page_installation_summary;
route(?PAGE_INSTALLATION_SUCCESS) -> page_installation_success;

%% Registration pages
route(?PAGE_REGISTRATION_SUMMARY) -> page_registration_summary;
route(?PAGE_REGISTRATION_SUCCESS) -> page_registration_success;

%% Spaces pages
route(?PAGE_SPACE_DETAILS) -> page_space_details;
route(?PAGE_SPACES_ACCOUNT) -> page_spaces_account;
route(?PAGE_SPACES_MANAGEMENT) -> page_spaces_management;

%% Undefined pages
route(_) -> page_404.