%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page displays registration summary and starts registration process.
%% @end
%% ===================================================================
-module(page_registration_summary).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).


%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY, ?PAGE_SPACES_ACCOUNT) of
                true ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
            end;
        false ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


title() ->
    <<"Registration">>.


body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Registration">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link, Breadcrumbs),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Registration">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Your software configuration has been successfully verified.<br>"
                "Please enter your <i>onedata</i> provider name and optional approximate geographical location.">>
            },
            #panel{
                body = #textbox{
                    id = <<"onezone_domain">>,
                    placeholder = <<"Onezone domain">>,
                    style = <<"margin: 0 auto; width: 25%;">>
                }
            },
            #panel{
                style = <<"margin-top: 1em;">>,
                body = #textbox{
                    id = <<"redirection_point">>,
                    placeholder = <<"Redirection point">>,
                    style = <<"margin: 0 auto; width: 25%;">>
                }
            },
            #panel{
                style = <<"margin-top: 1em;">>,
                body = #textbox{
                    id = <<"client_name">>,
                    placeholder = <<"Provider name">>,
                    style = <<"margin: 0 auto; width: 25%;">>
                }
            },
            #panel{
                style = <<"margin-top: 1em;">>,
                body = #textbox{
                    id = <<"geo_latitude">>,
                    placeholder = <<"Latitude (optional)">>,
                    style = <<"margin: 0 auto; width: 25%;">>
                }
            },
            #panel{
                style = <<"margin-top: 1em;">>,
                body = #textbox{
                    id = <<"geo_longitude">>,
                    placeholder = <<"Longitude (optional)">>,
                    style = <<"margin: 0 auto; width: 25%;">>
                }
            },
            #panel{
                id = <<"progress">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>,
                body = [
                    #panel{
                        body = #image{
                            style = <<"width: 2em;">>,
                            image = <<"/images/spinner.gif">>
                        }
                    },
                    #p{
                        style = <<"margin-left: 1em;">>,
                        body = <<"Registering...">>
                    }
                ]
            },
            onepanel_gui_utils:nav_buttons([
                {<<"register_button">>, {actions, gui_jq:form_submit_action(<<"register_button">>,
                    register, [<<"onezone_domain">>, <<"redirection_point">>, <<"client_name">>, <<"geo_latitude">>, <<"geo_longitude">>])},
                    false, <<"Register">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main).


%% ====================================================================
%% Events handling
%% ====================================================================

comet_loop_init() ->
    process_flag(trap_exit, true),
    comet_loop(#{pid => undefined}).


comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#{pid := Pid} = State) ->
    NewState =
        try
            receive
                {register, <<>>, _, _, _, _} ->
                    onepanel_gui_utils:message(error, <<"Please enter onezone domain.">>),
                    gui_jq:focus(<<"onezone_domain">>),
                    State;

                {register, _, <<>>, _, _, _} ->
                    onepanel_gui_utils:message(error, <<"Please enter redirection point.">>),
                    gui_jq:focus(<<"redirection_point">>),
                    State;

                {register, _, _, <<>>, _, _} ->
                    onepanel_gui_utils:message(error, <<"Please enter provider name.">>),
                    gui_jq:focus(<<"client_name">>),
                    State;

                {register, OzDomain, RedirectionPoint, ClientName, <<>>, Longitude} ->
                    self() ! {register, OzDomain, RedirectionPoint, ClientName, 0.0, Longitude},
                    State;

                {register, OzDomain, RedirectionPoint, ClientName, Latitude, <<>>} ->
                    self() ! {register, OzDomain, RedirectionPoint, ClientName, Latitude, 0.0},
                    State;


                {register, OzDomain, RedirectionPoint, ClientName, Latitude, Longitude} when is_binary(Latitude) ->
                    try
                        self() ! {register, OzDomain, RedirectionPoint, ClientName, binary_to_float(Latitude), Longitude},
                        gui_jq:focus(<<"geo_latitude">>)
                    catch
                        _:_ ->
                            onepanel_gui_utils:message(error, <<"Latitude should be a floating point number.">>)
                    end,
                    State;

                {register, OzDomain, RedirectionPoint, ClientName, Latitude, Longitude} when is_binary(Longitude) ->
                    try
                        self() ! {register, OzDomain, RedirectionPoint, ClientName, Latitude, binary_to_float(Longitude)},
                        gui_jq:focus(<<"geo_longitude">>)
                    catch
                        _:_ ->
                            onepanel_gui_utils:message(error, <<"Longitude should be a floating point number.">>)
                    end,
                    State;

                {register, OzDomain, RedirectionPoint, ClientName, Latitude, Longitude} when is_float(Latitude) and is_float(Longitude) ->
                    gui_jq:show(<<"progress">>),
                    gui_jq:hide(<<"onezone_domain">>),
                    gui_jq:hide(<<"redirection_point">>),
                    gui_jq:hide(<<"client_name">>),
                    gui_jq:hide(<<"geo_latitude">>),
                    gui_jq:hide(<<"geo_longitude">>),
                    gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"disabled">>),

                    NewPid = spawn_link(fun() ->
                        ok = onepanel_gui_logic:register_provider([
                            {onezoneDomainName, OzDomain},
                            {name, ClientName},
                            {redirectionPoint, RedirectionPoint},
                            {geoLatitude, Latitude},
                            {geoLongitude, Longitude}
                        ])
                    end),
                    State#{pid => NewPid};

                {'EXIT', Pid, normal} ->
                    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUCCESS),
                    State;

                {'EXIT', Pid, Reason} ->
                    case Reason of
                        {{badmatch,{error,Description}}, _} ->
                            onepanel_gui_utils:message(error, Description);
                        _ ->
                            onepanel_gui_utils:message(error, <<"Cannot register in <i>onezone</i>.<br>Please try again later.">>)
                    end,
                    gui_jq:hide(<<"progress">>),
                    gui_jq:show(<<"onezone_domain">>),
                    gui_jq:show(<<"redirection_point">>),
                    gui_jq:show(<<"client_name">>),
                    gui_jq:show(<<"geo_latitude">>),
                    gui_jq:show(<<"geo_longitude">>),
                    gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"">>),
                    State;

                _ ->
                    State

            after ?COMET_PROCESS_RELOAD_DELAY ->
                State
            end
        catch Type:Message ->
            ?error_stacktrace("Comet process exception: ~p:~p", [Type, Message]),
            onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
            {error, Message}
        end,
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


event(init) ->
    gui_jq:focus(<<"onezone_domain">>),
    gui_jq:bind_enter_to_change_focus(<<"onezone_domain">>, <<"redirection_point">>),
    gui_jq:bind_enter_to_change_focus(<<"redirection_point">>, <<"client_name">>),
    gui_jq:bind_enter_to_change_focus(<<"client_name">>, <<"geo_latitude">>),
    gui_jq:bind_enter_to_change_focus(<<"geo_latitude">>, <<"geo_longitude">>),
    gui_jq:bind_enter_to_submit_button(<<"geo_longitude">>, <<"register_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop_init() end),
    put(comet, Pid);

event(register) ->
    OzDomain = gui_ctx:postback_param(<<"onezone_domain">>),
    RedirectionPoint = gui_ctx:postback_param(<<"redirection_point">>),
    ClientName = gui_ctx:postback_param(<<"client_name">>),
    Latitude = gui_ctx:postback_param(<<"geo_latitude">>),
    Longitude = gui_ctx:postback_param(<<"geo_longitude">>),
    get(comet) ! {register, OzDomain, RedirectionPoint, ClientName, Latitude, Longitude};

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.
