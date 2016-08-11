%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page is displayed when client asks for not existing resource.
%% @end
%% ===================================================================
-module(page_404).

-include("modules/common.hrl").

-export([main/0, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


title() ->
    <<"Error 404">>.


body() ->
    Header = [],
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #panel{
                style = <<"width: 50%; margin: 0 auto;">>,
                class = <<"alert alert-danger">>,
                body = [
                    #h3{
                        body = <<"Error 404">>
                    },
                    #p{
                        style = <<"margin-bottom: 2em;">>,
                        body = <<"Requested page could not be found on the server.">>
                    },
                    #link{
                        id = <<"to_login_button">>,
                        postback = to_login,
                        class = <<"btn btn-warning btn-block">>,
                        style = <<"width: 8em; font-weight: bold; margin: 0 auto;">>,
                        body = <<"Main page">>
                    }
                ]
            },
            gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY)
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    ok;

event(to_login) ->
    gui_jq:redirect_to_login();

event(terminate) ->
    ok.