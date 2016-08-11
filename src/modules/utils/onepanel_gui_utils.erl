%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================
-module(onepanel_gui_utils).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([body/1, body/2, body/3, account_settings_tab/1, logotype_footer/0, nav_buttons/1, nav_buttons/2]).
-export([collapse_button/1, collapse_button/2, expand_button/1, expand_button/2, breadcrumbs/1]).
-export([format_list/1, message/2, message/4]).
-export([change_page/2, maybe_redirect/3, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

body(Main) ->
    body([], Main).


body(Header, Main) ->
    body(61, Header, Main).


body(Top, Header, Main) ->
    [
        #header{id = <<"page-header">>, class = <<"page-row">>, body = Header},
        #main{id = <<"page-main">>, class = <<"page-row page-row-expanded">>,
            body = #panel{
                style = <<"margin-top: ", (integer_to_binary(Top))/binary, "px; padding-top: 1px;">>,
                body = [
                    #panel{
                        id = <<"message">>,
                        style = <<"width: 100%; padding: 0.5em 0; margin: 0 auto; border: 0; display: none;">>,
                        class = <<"dialog">>
                    },
                    Main
                ]
            }
        }
    ].


breadcrumbs(Elements) ->
    [{LastElementName, LastElementCtx, LastElementPage} | ReversedTail] = lists:reverse(Elements),
    [
        #panel{
            class = <<"navbar-inner">>,
            style = <<"border-bottom: 1px solid gray;">>,
            body = #panel{
                class = <<"container">>,
                body = #list{
                    style = <<"margin: 0 auto; background-color: inherit;">>,
                    class = <<"breadcrumb">>,
                    body = lists:map(fun({Name, Ctx, Page}) ->
                        #li{
                            body = #link{
                                class = <<"glyph-link">>,
                                delegate = ?MODULE,
                                postback = {change_page, Ctx, Page},
                                body = Name
                            }
                        }
                    end, lists:reverse(ReversedTail)) ++ [
                        #li{
                            class = <<"active">>,
                            body = #link{
                                style = <<"color: #1abc9c">>,
                                class = <<"glyph-link">>,
                                delegate = ?MODULE,
                                postback = {change_page, LastElementCtx, LastElementPage},
                                body = LastElementName
                            }
                        }
                    ]
                }
            }
        }
    ].


logotype_footer() ->
    #panel{style = <<"text-align: center; margin-top: 2em; margin-bottom: 2em;">>,
        body = [
            #image{class = <<"pull-left">>, image = <<"/images/innow-gosp-logo.png">>},
            #image{image = <<"/images/plgrid-plus-logo.png">>},
            #image{class = <<"pull-right">>, image = <<"/images/unia-logo.png">>}
        ]
    }.


nav_buttons(Buttons) ->
    nav_buttons(Buttons, <<"50%">>).


nav_buttons(Buttons, Width) ->
    ButtonClass = <<"btn btn-inverse btn-small">>,
    ButtonStyle = <<"min-width: 8em; margin-left: 1em; margin-right: 1em; font-weight: bold;">>,
    #panel{
        style = <<"width: ", Width/binary, "; margin: 0 auto; margin-top: 3em;">>,
        body = lists:map(fun
            ({Id, {postback, Postback}, Disabled, Body}) ->
                #button{
                    id = Id,
                    postback = Postback,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            ({Id, {actions, Actions}, Disabled, Body}) ->
                #button{
                    id = Id,
                    actions = Actions,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            ({Id, _, Disabled, Body}) ->
                #button{
                    id = Id,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            (_) ->
                #button{}
        end, Buttons)
    }.


collapse_button(Postback) ->
    collapse_button(<<"Collapse">>, Postback).


collapse_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large; vertical-align: top;">>,
            class = <<"fui-triangle-up">>
        }
    }.


expand_button(Postback) ->
    expand_button(<<"Expand">>, Postback).


expand_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large;  vertical-align: top;">>,
            class = <<"fui-triangle-down">>
        }
    }.


account_settings_tab(Username) ->
    #link{
        style = <<"padding: 18px;">>,
        title = <<"Account settings">>,
        url = ?PAGE_ACCOUNT_SETTINGS,
        body = [
            Username,
            #span{
                class = <<"fui-user">>,
                style = <<"margin-left: 10px;">>
            }
        ]
    }.


change_page(Ctx, Page) ->
    gui_ctx:put(Ctx, Page),
    gui_jq:redirect(Page).


maybe_redirect(Env, CurrentPage, DefaultPage) ->
    case gui_ctx:get(Env) of
        CurrentPage ->
            false;
        undefined ->
            gui_jq:redirect(DefaultPage),
            true;
        Page ->
            gui_jq:redirect(Page),
            true
    end.


event({change_page, Ctx, Page}) ->
    change_page(Ctx, Page).


format_list([]) ->
    <<"">>;

format_list(Hosts) ->
    list_to_binary(string:join(Hosts, ", ")).


message(Type, Message) ->
    message(<<"message">>, Type, Message, {close_message, <<"message">>}).


message(Id, Type, Message, Postback) ->
    Body = [
        Message,
        #link{
            id = <<"close_message_button">>,
            title = <<"Close">>,
            style = <<"position: absolute; top: 0.5em; right: 0.5em;">>,
            class = <<"glyph-link">>,
            postback = Postback,
            body = #span{
                class = <<"fui-cross">>
            }
        }
    ],
    case Type of
        success ->
            gui_jq:add_class(Id, <<"dialog-success">>),
            gui_jq:remove_class(Id, <<"dialog-danger">>);
        _ ->
            gui_jq:add_class(Id, <<"dialog-danger">>),
            gui_jq:remove_class(Id, <<"dialog-success">>)
    end,
    gui_jq:update(Id, Body),
    gui_jq:fade_in(Id, 300).