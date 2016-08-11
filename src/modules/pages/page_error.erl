%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
%% The page is displayed when an error occurs.
%% @end
%% ===================================================================
-module(page_error).

-include("modules/common.hrl").

%% n2o API
-export([main/0, event/1]).

%% API
-export([redirect_with_error/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


title() -> <<"Error">>.


body() ->
    {Reason, Description} = get_reason_and_description(),
    Header = [],
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #panel{
                style = <<"width: 50%; margin: 0 auto;">>,
                class = <<"alert alert-danger">>,
                body = [
                    #h3{
                        body = Reason
                    },
                    #p{
                        style = <<"margin-bottom: 2em;">>,
                        body = Description
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


redirect_with_error(ErrorId) ->
    gui_jq:redirect(<<"/error?id=", ErrorId/binary>>).


get_reason_and_description() ->
    ErrorId = str_utils:to_binary(gui_ctx:url_param(<<"id">>)),
    id_to_reason_and_message(ErrorId).


id_to_reason_and_message(ErrorId) ->
    UnknownError = {
        <<"Unknown error">>,
        <<"">>
    },
    proplists:get_value(ErrorId, ?ERROR_MESSAGES, UnknownError).


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"to_login_button">>),
    ok;

event(to_login) ->
    gui_jq:redirect_to_login();

event(terminate) ->
    ok.
