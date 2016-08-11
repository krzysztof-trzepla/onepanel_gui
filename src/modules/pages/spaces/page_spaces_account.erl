%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to manage provider account.
%% @end
%% ===================================================================
-module(page_spaces_account).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, api_event/3, comet_loop/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case onepanel_gui_logic:get_hosts(workers) of
                {error, not_found} ->
                    page_error:redirect_with_error(?SOFTWARE_NOT_INSTALLED_ERROR),
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    case gui_ctx:get(?CURRENT_REGISTRATION_PAGE) of
                        undefined ->
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, custom()}]};
                        Page ->
                            gui_jq:redirect(Page),
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
                    end
            end;
        false ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


title() ->
    <<"Account settings">>.


custom() ->
    <<"<script src='/flatui/bootbox.min.js' type='text/javascript' charset='utf-8'></script>">>.


body() ->
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Account settings">>
            },
            #panel{
                id = <<"account_table">>
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


account_table(ProviderDetails) ->
    #table{
        style = <<"border-width: 0; width: 100%;">>,
        body = lists:map(fun({TooltipId, TooltipStyle, TooltipBody, LabelId, LabelBody, CellId, CellBody}) ->
            #tr{
                cells = [
                    #td{
                        style = <<"border-width: 0; text-align: right; padding: 1em 1em; width: 50%; vertical-align: top; position: relative;">>,
                        body = [
                            #panel{
                                id = TooltipId,
                                class = <<"tooltip left in tooltip-light">>,
                                style = TooltipStyle,
                                body = [
                                    #panel{
                                        class = <<"tooltip-arrow">>
                                    },
                                    #panel{
                                        class = <<"tooltip-inner">>,
                                        style = <<"font-size: small; width: 200px;">>,
                                        body = TooltipBody
                                    }
                                ]
                            },
                            #flatui_label{
                                id = LabelId,
                                style = <<"margin: 0 auto; cursor: auto;">>,
                                class = <<"label label-large label-inverse label-tooltip">>,
                                body = LabelBody
                            }
                        ]
                    },
                    #td{
                        id = CellId,
                        style = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
                        body = CellBody
                    }
                ]
            }
        end, [
            {<<"provider_id_tooltip">>, <<"top: 0px; right: 110px; display: none;">>, <<"Globally unique identifier assigned by <i>onezone</i>.">>,
                <<"provider_id_label">>, <<"Provider ID">>, <<"provider_id">>, providerId(ProviderDetails)},
            {<<"provider_name_tooltip">>, <<"top: 5px; right: 77px; display: none;">>, <<"Provider's name in <i>onezone</i>.">>,
                <<"provider_name_label">>, <<"Name">>, <<"provider_name">>, provider_name(ProviderDetails)},
            {<<"urls_tooltip">>, <<"top: -10px; right: 73px; display: none;">>, <<"List of <i>worker</i> components' IP addresses visible for <i>onezone</i>.">>,
                <<"urls_label">>, <<"URLs">>, <<"urls">>, urls(ProviderDetails)},
            {<<"redirection_point_tooltip">>, <<"top: -10px; right: 143px; display: none;">>, <<"Web address used by <i>onezone</i> to redirect users to provider.">>,
                <<"redirection_point_label">>, <<"Redirection point">>, <<"redirection_point">>, redirection_point(ProviderDetails)}
        ])
    }.


providerId(#{<<"id">> := ProviderId}) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            ProviderId,
            #link{
                title = <<"Unregister">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = unregister,
                body = #span{
                    class = <<"fui-cross-inverted">>
                }
            }
        ]
    };

providerId(_) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>,
            #link{
                id = <<"register_link">>,
                title = <<"Register">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = register,
                body = #span{
                    class = <<"fui-plus-inverted">>
                }
            }
        ]
    }.


provider_name(#{<<"name">> := ProviderName}) ->
    #span{
        style = <<"font-size: large;">>,
        body = ProviderName
    };

provider_name(_) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    }.


urls(#{<<"urls">> := []}) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    };

urls(#{<<"urls">> := URLs}) ->
    #list{
        style = <<"list-style-type: none; margin: 0 auto;">>,
        body = lists:map(fun(URL) ->
            #li{
                body = #span{
                    style = <<"font-size: large;">>,
                    body = URL
                }
            }
        end, lists:sort(URLs))
    };

urls(_) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    }.


redirection_point(#{<<"redirectionPoint">> := RedirectionPoint}) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            http_utils:html_encode(RedirectionPoint),
            #link{
                title = <<"Edit">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = {change_redirection_point, RedirectionPoint},
                body = #span{
                    class = <<"icomoon-pencil2">>
                }
            }
        ]
    };

redirection_point(_) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    }.


change_redirection_point(RedirectionPoint) ->
    [
        #textbox{
            id = <<"new_redirection_point_textbox">>,
            style = <<"margin: 0 auto; padding: 1px;">>,
            class = <<"span">>,
            placeholder = <<"New redirection point">>
        },
        #link{
            id = <<"new_redirection_point_submit">>,
            style = <<"margin-left: 10px;">>,
            class = <<"glyph-link">>,
            title = <<"Submit">>,

            actions = gui_jq:form_submit_action(<<"new_redirection_point_submit">>,
                {submit_new_redirection_point, RedirectionPoint}, <<"new_redirection_point_textbox">>),
            body = #span{
                class = <<"fui-check-inverted">>,
                style = <<"font-size: large; vertical-align: middle;">>
            }
        },
        #link{
            style = <<"margin-left: 10px;">>,
            class = <<"glyph-link">>,
            title = <<"Cancel">>,
            postback = {cancel_new_redirection_point_submit, RedirectionPoint},
            body = #span{
                class = <<"fui-cross-inverted">>,
                style = <<"font-size: large; vertical-align: middle;">>
            }
        }
    ].


%% ====================================================================
%% Events handling
%% ====================================================================

comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(_) ->
    NewState = try
        receive
            unregister ->
                case onepanel_gui_logic:unregister_provider() of
                    ok ->
                        gui_jq:update(<<"provider_id">>, providerId(undefined)),
                        gui_jq:update(<<"provider_name">>, provider_name(undefined)),
                        gui_jq:update(<<"urls">>, urls(undefined)),
                        gui_jq:update(<<"redirection_point">>, redirection_point(undefined)),
                        onepanel_gui_utils:message(success, <<"You have been successfully unregistered from <i>onezone</i>.">>);
                    {error, Reason} ->
                        onepanel_gui_utils:message(error, Reason)
                end;

            {change_redirection_point, _, <<>>} ->
                onepanel_gui_utils:message(error, <<"Redirection point cannot be empty.">>);

            {change_redirection_point, RedirectionPoint, RedirectionPoint} ->
                gui_jq:update(<<"redirection_point">>, redirection_point(RedirectionPoint));

            {change_redirection_point, OldRedirectionPoint, RedirectionPoint} ->
                case onepanel_gui_logic:modify_details([{redirectionPoint, RedirectionPoint}]) of
                    ok ->
                        gui_jq:update(<<"redirection_point">>, redirection_point(#{<<"redirectionPoint">> => RedirectionPoint})),
                        onepanel_gui_utils:message(success, <<"Redirection point changed successfully.">>);
                    {error, Reason} ->
                        gui_jq:update(<<"redirection_point">>, redirection_point(OldRedirectionPoint)),
                        onepanel_gui_utils:message(error, Reason)
                end
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
    try
        {ok, ProviderDetails} = onepanel_gui_logic:get_provider_details(),
        gui_jq:update(<<"account_table">>, account_table(ProviderDetails)),
        gui_jq:hide(<<"main_spinner">>),

        gui_jq:wire(#api{name = "unregister", tag = "unregister"}, false),
        gui_jq:bind_key_to_click(<<"13">>, <<"new_redirection_point_submit">>),
        lists:foreach(fun({LabelId, TooltipId}) ->
            gui_jq:wire(<<"$('#", LabelId/binary, "').hover(function() {"
            "       $('#", TooltipId/binary, "').show();"
            "   }, function() {"
            "       $('#", TooltipId/binary, "').hide();"
            "   }"
            ");">>)
        end, [
            {<<"provider_id_label">>, <<"provider_id_tooltip">>},
            {<<"provider_name_label">>, <<"provider_name_tooltip">>},
            {<<"urls_label">>, <<"urls_tooltip">>},
            {<<"redirection_point_label">>, <<"redirection_point_tooltip">>}
        ]),

        {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#{}) end),
        put(comet, Pid)
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(error, <<"Cannot fetch provider details.<br>Please try again later.">>)
    end;

event(to_root_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(register) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY);

event(unregister) ->
    Title = <<"Unregister">>,
    Message = <<"Are you sure you want to unregister from <i>onezone</i>?">>,
    Script = <<"unregister();">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass);

event({change_redirection_point, RedirectionPoint}) ->
    gui_jq:update(<<"redirection_point">>, change_redirection_point(RedirectionPoint)),
    gui_jq:focus(<<"new_redirection_point_textbox">>);

event({submit_new_redirection_point, RedirectionPoint}) ->
    NewRedirectionPoint = gui_ctx:postback_param(<<"new_redirection_point_textbox">>),
    get(comet) ! {change_redirection_point, RedirectionPoint, NewRedirectionPoint},
    gui_jq:show(<<"main_spinner">>);

event({cancel_new_redirection_point_submit, RedirectionPoint}) ->
    gui_jq:update(<<"redirection_point">>, redirection_point(#{<<"redirectionPoint">> => RedirectionPoint}));

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.


api_event("unregister", _, _) ->
    get(comet) ! unregister,
    gui_jq:show(<<"main_spinner">>).