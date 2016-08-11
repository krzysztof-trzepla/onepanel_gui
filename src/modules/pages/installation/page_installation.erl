%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page is a starting point for software components installation.
%% @end
%% ===================================================================
-module(page_installation).

-include("modules/common.hrl").

-export([main/0, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case gui_ctx:get(?CURRENT_INSTALLATION_PAGE) of
                undefined ->
                    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION);
                Page ->
                    gui_jq:redirect(Page)
            end,
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, <<"">>}, {custom, <<"">>}]};
        false ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


title() ->
    <<"Installation">>.


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    ok;

event(terminate) ->
    ok.