%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains n2o website code.
%% This page handles user validation via OpenID.
%% @end
%% ===================================================================
-module(page_login_validation).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


title() -> <<"Login validation">>.


body() ->
    case gui_ctx:user_logged_in() of
        true -> gui_jq:redirect(?PAGE_ROOT);
        false ->
            Params = gui_ctx:form_params(),
            Username = proplists:get_value(<<"username">>, Params),
            Password = proplists:get_value(<<"password">>, Params),
            case onepanel_gui_logic:login(Username, Password) of
                ok ->
                    ?info("Successful login of user: ~p", [Username]),
                    gui_ctx:create_session(),
                    gui_ctx:set_user_id(Username),
                    ets:insert(store, {user_auth, {Username, base64:encode(Password)}}),
                    gui_jq:redirect_from_login();
                {error, Reason} -> page_error:redirect_with_error(Reason)
            end
    end,
    [].


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    ok;

event(terminate) ->
    ok.