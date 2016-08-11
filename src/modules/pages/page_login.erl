%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page handles users' logging in.
%% @end
%% ===================================================================
-module(page_login).

-include("modules/common.hrl").

-export([main/0, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


title() ->
    <<"Log in">>.


body() ->
    case gui_ctx:user_logged_in() of
        true -> gui_jq:redirect(?PAGE_ROOT);
        false -> render_login_page()
    end.


render_login_page() ->
    Header = [],
    Main = [
        #panel{
            class = <<"alert alert-success">>,
            style = <<"width: 30em; margin: 0 auto; text-align: center; margin-top: 2em;">>,
            body = [
                #h3{
                    body = <<"Welcome to onepanel">>
                },
                #form{
                    id = <<"login_form">>,
                    method = "post",
                    action = ?PAGE_LOGIN_VALIDATION,
                    style = <<"width: 15em; margin: 0 auto; padding-top: 1em; float: center">>,
                    body = [
                        #textbox{
                            id = <<"username">>,
                            name = <<"username">>,
                            class = <<"span">>,
                            placeholder = <<"Username">>
                        },
                        #password{
                            id = <<"password">>,
                            name = <<"password">>,
                            class = <<"span">>,
                            placeholder = <<"Password">>
                        },
                        #button{
                            id = <<"login_button">>,
                            type = <<"submit">>,
                            class = <<"btn btn-primary btn-block">>,
                            style = <<"margin: 0 auto;">>,
                            body = <<"Log in">>
                        }
                    ]
                }
            ]
        },
        gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY)
    ],
    onepanel_gui_utils:body(Header, Main).


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    gui_jq:focus(<<"username">>),
    gui_jq:bind_enter_to_change_focus(<<"username">>, <<"password">>),
    gui_jq:bind_enter_to_submit_button(<<"password">>, <<"login_button">>),
    ok;

event(refresh) ->
    gui_jq:redirect(<<"/login">>);

event(terminate) ->
    ok.