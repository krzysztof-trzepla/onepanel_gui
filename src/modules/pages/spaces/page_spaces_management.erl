%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to manage provider spaces.
%% @end
%% ===================================================================
-module(page_spaces_management).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, api_event/3, comet_loop/1]).

-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    try
        case gui_ctx:user_logged_in() of
            true ->
                case onepanel_gui_logic:get_provider_id() of
                    undefined ->
                        page_error:redirect_with_error(?UNREGISTERED_PROVIDER_ERROR),
                        #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                    _ ->
                        #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, custom()}]}
                end;
            false ->
                gui_jq:redirect_to_login(),
                #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
        end
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>),
            []
    end.

title() ->
    <<"Spaces management">>.


custom() ->
    <<
        "<script src='/flatui/bootbox.min.js' type='text/javascript' charset='utf-8'></script>",
        "<script src='/js/spaces_management.js' type='text/javascript' charset='utf-8'></script>"
    >>.


body() ->
    {ok, Storages} = onepanel_gui_logic:get_storages(),
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_dashboard_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Spaces settings">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Supported <i>Spaces</i> are presented in the table below.">>
            },
            onepanel_gui_utils:nav_buttons([
                {<<"create_space_button">>, {postback, {create_space, Storages}}, true, <<"Create Space">>},
                {<<"support_space_button">>, {postback, {support_space, Storages}}, true, <<"Support Space">>}
            ], <<"20em">>),
            #table{
                class = <<"table table-bordered table-striped">>,
                style = <<"width: 50%; margin: 0 auto; margin-top: 3em; table-layout: fixed;">>,
                body = #tbody{
                    id = <<"spaces_table">>,
                    style = <<"display: none;">>
                }
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


spaces_table_collapsed(SpacesDetails) ->
    NavigationBody = onepanel_gui_utils:expand_button(<<"Expand All">>, {message, expand_spaces_table}),
    RenderRowFunction = fun space_row_collapsed/2,
    spaces_table(SpacesDetails, NavigationBody, RenderRowFunction).


spaces_table_expanded(SpacesDetails) ->
    NavigationBody = onepanel_gui_utils:collapse_button(<<"Collapse All">>, {message, collapse_spaces_table}),
    RenderRowFunction = fun space_row_expanded/2,
    spaces_table(SpacesDetails, NavigationBody, RenderRowFunction).


spaces_table(SpacesDetails, NavigationBody, RenderRowFunction) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Spaces">>
            },
            #th{
                style = ?NAVIGATION_COLUMN_STYLE,
                body = NavigationBody
            }
        ]
    },

    Rows = lists:foldl(fun({RowId, SpaceDetails}, RowsAcc) ->
        [#tr{
            id = RowId,
            cells = RenderRowFunction(RowId, SpaceDetails)
        } | RowsAcc]
    end, [], SpacesDetails),

    [Header | Rows].


space_row_collapsed(RowId, #{<<"id">> := SpaceId, <<"name">> := SpaceName} = SpaceDetails) ->
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #p{
                style = ?PARAGRAPH_STYLE,
                body = <<"<b>", SpaceName/binary, "</b> (", SpaceId/binary, ")">>
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = onepanel_gui_utils:expand_button({message, {expand_space_row, RowId, SpaceDetails}})
        }
    ].


space_row_expanded(RowId, #{<<"id">> := SpaceId, <<"name">> := SpaceName} = SpaceDetails) ->
    SettingsIcons = lists:map(fun({LinkTitle, LinkPostback, SpanClass}) ->
        #link{
            title = LinkTitle,
            style = <<"font-size: large; margin-right: 1em;">>,
            class = <<"glyph-link">>,
            postback = LinkPostback,
            body = #span{
                class = SpanClass
            }
        }
    end, [
%%        {<<"Get details">>, {get_details, SpaceDetails}, <<"fui-info">>},
        {<<"Revoke support">>, {revoke_space_support, RowId, SpaceDetails}, <<"fui-trash">>}
    ]),
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = <<"border-width: 0; width: 100%; border-collapse: inherit;">>,
                body = lists:map(fun({Description, Main}) ->
                    #tr{
                        cells = [
                            #td{
                                style = <<"border-width: 0; text-align: right; width: 10%; padding-left: 0; padding-right: 0;">>,
                                body = #label{
                                    style = <<"margin: 0 auto; cursor: auto;">>,
                                    class = <<"label label-large label-inverse">>,
                                    body = Description
                                }
                            },
                            #td{
                                style = <<"border-width: 0;  text-align: left; padding-left: 1em; width: 90%;">>,
                                body = #p{
                                    style = ?PARAGRAPH_STYLE,
                                    body = Main
                                }
                            }
                        ]
                    }
                end, [
                    {<<"Name">>, SpaceName},
                    {<<"Space ID">>, SpaceId},
                    {<<"Settings">>, SettingsIcons}
                ])
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = onepanel_gui_utils:collapse_button({message, {collapse_space_row, RowId, SpaceDetails}})
        }
    ].


add_space_row(RowId, SpaceDetails) ->
    Row = #tr{
        id = RowId,
        cells = space_row_collapsed(RowId, SpaceDetails)
    },
    gui_jq:insert_bottom(<<"spaces_table">>, Row).


storage_dropdown(Storages, Form) ->
    Options = lists:map(fun({SName, #{<<"id">> := StorageId}}) ->
        <<"<option value=\"", StorageId/binary, "\">", SName/binary, "</option>">>
    end, Storages),
    Header = <<"<div id=\"storage_list\" style=\"display: flex;\">",
        "<label for=\"storage_id\" style=\"font-size: larger; line-height: 2em; "
        "padding-right: 0.5em;\">Storage:</label>",
        "<select id=\"storage_id\" class=\"select\" onchange=(function(){",
        "$(\"#storage_form\").remove();",
        Form/binary,
        "})()>">>,
    lists:foldl(fun(Part, Acc) ->
        <<Acc/binary, Part/binary>>
    end, Header, Options ++ [<<"</select></div>">>]).

%% ====================================================================
%% Events handling
%% ====================================================================


comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#{counter := Counter, spaces_details := SpacesDetails} = State) ->
    NewState = try
        receive
            {create_space, StorageId, Name, Token, Size} ->
                NextState =
                    try
                        RowId = <<"space_", (integer_to_binary(Counter + 1))/binary>>,
                        {ok, SpaceId} = onepanel_gui_logic:support_space([
                            {name, Name}, {token, Token}, {size, Size}, {storageId, StorageId}
                        ]),
                        {ok, SpaceDetails} = onepanel_gui_logic:get_space_details(SpaceId),
                        add_space_row(RowId, SpaceDetails),
                        onepanel_gui_utils:message(success, <<"Created Space's ID: <b>", SpaceId/binary, "</b>">>),
                        State#{counter => Counter + 1, spaces_details => [{RowId, SpaceDetails} | SpacesDetails]}
                    catch
                        _:Reason ->
                            ?error_stacktrace("Cannot create Space ~p associated with token ~p: ~p", [Name, Token, Reason]),
                            onepanel_gui_utils:message(error, <<"Cannot create Space <b>", Name/binary, "</b> associated with token <b>", Token/binary, "</b>.<br>
                            Please try again later.">>),
                            State
                    end,
                gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"">>),
                NextState;

            {support_space, StorageId, Token, Size} ->
                NextState =
                    try
                        RowId = <<"space_", (integer_to_binary(Counter + 1))/binary>>,
                        {ok, SpaceId} = onepanel_gui_logic:support_space([
                            {token, Token}, {size, Size}, {storageId, StorageId}
                        ]),
                        {ok, SpaceDetails} = onepanel_gui_logic:get_space_details(SpaceId),
                        add_space_row(RowId, SpaceDetails),
                        onepanel_gui_utils:message(success, <<"Supported Space's ID: <b>", SpaceId/binary, "</b>">>),
                        State#{counter => Counter + 1, spaces_details => [{RowId, SpaceDetails} | SpacesDetails]}
                    catch
                        _:Reason ->
                            ?error_stacktrace("Cannot support Space associated with token ~p: ~p", [Token, Reason]),
                            onepanel_gui_utils:message(error, <<"Cannot support Space associated with token <b>", Token/binary, "</b>.<br>
                            Please try again later.">>),
                            State
                    end,
                gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"">>),
                NextState;

            {revoke_space_support, RowId, SpaceId} ->
                NextState =
                    case onepanel_gui_logic:revoke_space_support(SpaceId) of
                        ok ->
                            onepanel_gui_utils:message(success, <<"Space: <b>", SpaceId/binary, "</b> is no longer supported.">>),
                            gui_jq:remove(RowId),
                            State#{spaces_details => proplists:delete(RowId, SpacesDetails)};
                        Other ->
                            ?error("Cannot revoke support for Space ~p: ~p", [SpaceId, Other]),
                            onepanel_gui_utils:message(error, <<"Cannot revoke support for Space <b>", SpaceId/binary, "</b>.<br>Please try again later.">>),
                            State
                    end,
                NextState;

            render_spaces_table ->
                gui_jq:update(<<"spaces_table">>, spaces_table_collapsed(SpacesDetails)),
                gui_jq:fade_in(<<"spaces_table">>, 500),
                gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"">>),
                State;

            Event ->
                case Event of
                    collapse_spaces_table ->
                        gui_jq:update(<<"spaces_table">>, spaces_table_collapsed(SpacesDetails));
                    expand_spaces_table ->
                        gui_jq:update(<<"spaces_table">>, spaces_table_expanded(SpacesDetails));
                    {collapse_space_row, RowId, SpaceDetails} ->
                        gui_jq:update(RowId, space_row_collapsed(RowId, SpaceDetails));
                    {expand_space_row, RowId, SpaceDetails} ->
                        gui_jq:update(RowId, space_row_expanded(RowId, SpaceDetails));
                    _ ->
                        ok
                end,
                State

        after ?COMET_PROCESS_RELOAD_DELAY ->
            State
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
        {ok, SpaceIds} = onepanel_gui_logic:get_spaces(),
        {SpacesDetails, Counter} = lists:foldl(fun(SpaceId, {SpacesDetailsAcc, Id}) ->
            {ok, SpaceDetails} = onepanel_gui_logic:get_space_details(SpaceId),
            {
                [{<<"space_", (integer_to_binary(Id + 1))/binary>>, SpaceDetails} | SpacesDetailsAcc],
                Id + 1
            }
        end, {[], 0}, SpaceIds),

        gui_jq:wire(#api{name = "create_space", tag = "create_space"}, false),
        gui_jq:wire(#api{name = "support_space", tag = "support_space"}, false),
        gui_jq:wire(#api{name = "revoke_space_support", tag = "revoke_space_support"}, false),
        gui_jq:bind_key_to_click_on_class(<<"13">>, <<"confirm">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#{counter => Counter, spaces_details => SpacesDetails})
        end),
        put(comet, Pid),
        Pid ! render_spaces_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch supported Spaces.<br>Please try again later.">>)
    end;

event({create_space, Storages}) ->
    Title = <<"Create Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
        "<p id=\"space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
        (storage_dropdown(Storages, <<"storage_create_form();">>))/binary,
        "</div>">>,
    Script = <<"return create_space_check();">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass),
    gui_jq:wire(<<"box.on('shown',storage_create_form);">>);

event({support_space, Storages}) ->
    Title = <<"Support Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
        "<p id=\"space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
        (storage_dropdown(Storages, <<"storage_support_form();">>))/binary,
        "</div>">>,
    Script = <<"return support_space_check();">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass),
    gui_jq:wire(<<"box.on('shown',storage_support_form);">>);

event({revoke_space_support, RowId, #{<<"id">> := SpaceId}}) ->
    Title = <<"Revoke Space support">>,
    Message = <<"Are you sure you want to stop supporting Space: <b>", SpaceId/binary, "</b>?<br>This operation cannot be undone.">>,
    Script = <<"revoke_space_support(['", SpaceId/binary, "','", RowId/binary, "']);">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass);

event({get_details, #{<<"id">> := SpaceId}}) ->
    gui_jq:redirect(<<"/spaces?id=", SpaceId/binary>>);

event({message, Message}) ->
    get(comet) ! Message,
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.


api_event("create_space", Args, _) ->
    [StorageId, Name, Token, Size] = mochijson2:decode(Args),
    get(comet) ! {create_space, StorageId, Name, Token, Size},
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("support_space", Args, _) ->
    [StorageId, Token, Size] = mochijson2:decode(Args),
    get(comet) ! {support_space, StorageId, Token, Size},
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("revoke_space_support", Args, _) ->
    [SpaceId, RowId] = mochijson2:decode(Args),
    get(comet) ! {revoke_space_support, RowId, SpaceId},
    gui_jq:show(<<"main_spinner">>).