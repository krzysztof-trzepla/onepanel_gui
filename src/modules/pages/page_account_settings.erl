%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows user to change username and password.
%% @end
%% ===================================================================
-module(page_account_settings).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([main/0, event/1, comet_loop/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
        _ ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


title() ->
    <<"Account settings">>.


body() ->
    Header = onepanel_gui_utils_adapter:top_menu(account_settings_tab),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Account settings">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Any change to username or password is also done for account in administrative database.">>
            },
            settings_table()
        ]
    },
    onepanel_gui_utils:body(Header, Main).


settings_table() ->
    RowStyle = <<"line-height: 4em;">>,
    DescriptionStyle = <<"border-width: 0; text-align: right; padding: 1em 1em; width: 50%;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: 100%;">>, body = [
            #tr{
                style = RowStyle,
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"Username">>
                        }
                    },
                    #td{
                        id = <<"username">>,
                        style = MainStyle,
                        body = username(gui_ctx:get_user_id())
                    }
                ]
            },
            #tr{
                style = RowStyle,
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"Password">>
                        }
                    },
                    #td{
                        id = <<"password">>,
                        style = MainStyle,
                        body = password()
                    }
                ]
            }
        ]
    }.


username(Username) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            http_utils:html_encode(Username)
        ]
    }.


password() ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            <<"&#9679&#9679&#9679&#9679&#9679&#9679&#9679&#9679">>,
            #link{
                title = <<"Edit">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = change_password,
                body = #span{
                    class = <<"fui-new">>
                }
            }
        ]
    }.


change_password() ->
    #panel{
        style = <<"width: 19em;">>,
        body = [
            #password{
                id = <<"current_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"Current password">>
            },
            #link{
                id = <<"new_password_submit">>,
                class = <<"glyph-link">>,
                style = <<"margin-left: 1em;">>,
                title = <<"Submit">>,
                actions = gui_jq:form_submit_action(<<"new_password_submit">>, submit_new_password,
                    [<<"current_password_textbox">>, <<"new_password_textbox">>, <<"confirmed_password_textbox">>]),
                body = #span{
                    class = <<"fui-check-inverted">>,
                    style = <<"font-size: large;">>
                }
            },
            #link{
                class = <<"glyph-link">>,
                style = <<"margin-left: 10px;">>,
                title = <<"Cancel">>,
                postback = cancel_new_password_submit,
                body = #span{
                    class = <<"fui-cross-inverted">>,
                    style = <<"font-size: large;">>
                }
            },
            #password{
                id = <<"new_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"New password">>
            },
            #password{
                id = <<"confirmed_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"Confirm password">>
            }
        ]
    }.

%% ====================================================================
%% Events handling
%% ====================================================================

comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(_) ->
    NewState = try
        receive
            {submit_new_password, Username, CurrentPassword, NewPassword, ConfirmedPassword} ->
                case onepanel_gui_logic:change_password(Username, CurrentPassword, NewPassword, ConfirmedPassword) of
                    ok ->
                        ets:insert(store, {user_auth, {Username, base64:encode(NewPassword)}}),
                        onepanel_gui_utils:message(success, "Password changed."),
                        gui_jq:update(<<"password">>, password());
                    {error, Reason} ->
                        onepanel_gui_utils:message(error, Reason)
                end

        after ?COMET_PROCESS_RELOAD_DELAY ->
            ok
        end
    catch Type:Message ->
        ?error_stacktrace("Comet process exception: ~p:~p", [Type, Message]),
        onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
        {error, Message}
    end,
    gui_jq:wire(<<"$('#main_spinner').delay(300).hide(0);">>, false),
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


event(init) ->
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#{}) end),
    put(comet, Pid),
    ok;

event(change_password) ->
    gui_jq:update(<<"password">>, change_password()),
    gui_jq:focus(<<"current_password_textbox">>),
    gui_jq:bind_enter_to_change_focus(<<"current_password_textbox">>, <<"new_password_textbox">>),
    gui_jq:bind_enter_to_change_focus(<<"new_password_textbox">>, <<"confirmed_password_textbox">>),
    gui_jq:bind_enter_to_submit_button(<<"confirmed_password_textbox">>, <<"new_password_submit">>);

event(cancel_new_password_submit) ->
    gui_jq:update(<<"password">>, password());

event(submit_new_password) ->
    Username = gui_ctx:get_user_id(),
    CurrentPassword = gui_ctx:postback_param(<<"current_password_textbox">>),
    NewPassword = gui_ctx:postback_param(<<"new_password_textbox">>),
    ConfirmedPassword = gui_ctx:postback_param(<<"confirmed_password_textbox">>),
    get(comet) ! {submit_new_password, Username, CurrentPassword, NewPassword, ConfirmedPassword},
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.
