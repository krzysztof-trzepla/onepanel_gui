%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to view space details.
%% @end
%% ===================================================================
-module(page_space_details).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/oz/oz_providers.hrl").

-export([main/0, event/1, comet_loop/1]).

-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case onepanel_utils:get_provider_id() of
                undefined ->
                    page_error:redirect_with_error(?UNREGISTERED_PROVIDER_ERROR),
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    case gui_ctx:url_param(<<"id">>) of
                        undefined ->
                            page_error:redirect_with_error(?SPACE_NOT_FOUND_ERROR),
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                        Id ->
                            SpaceId = str_utils:to_binary(Id),
                            case onepanel_gui_logic:get_space_details(SpaceId) of
                                {ok, SpaceDetails} ->
                                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body(SpaceDetails)}, {custom, <<"">>}]};
                                Other ->
                                    ?error("Cannot get details of Space with ID ~p: ~p", [SpaceId, Other]),
                                    page_error:redirect_with_error(?SPACE_PERMISSION_DENIED_ERROR),
                                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
                            end
                    end
            end;
        false ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


title() ->
    <<"Space details">>.


body(SpaceDetails) ->
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_dashboard_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 2em;">>,
                body = <<"Space details">>
            },
            space_details_table(SpaceDetails) |
            lists:map(fun(TableId) ->
                #table{
                    class = <<"table table-bordered table-striped">>,
                    style = <<"width: 50%; margin: 0 auto; margin-top: 3em; table-layout: fixed;">>,
                    body = #tbody{
                        id = TableId,
                        style = <<"display: none;">>
                    }
                }
            end, [<<"providers_table">>, <<"users_table">>])
        ]
    },
    onepanel_gui_utils:body(Header, Main).


space_details_table(#{<<"id">> := SpaceId, <<"name">> := SpaceName}) ->
    DescriptionStyle = <<"border-width: 0; vertical-align: top; text-align: right; padding: 1em 1em;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: 50%; margin: 0 auto;">>,
        body = lists:map(fun({DetailName, DetailValue}) ->
            #tr{
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            style = <<"cursor: auto;">>,
                            body = DetailName
                        }
                    },
                    #td{
                        style = MainStyle,
                        body = #p{
                            style = ?PARAGRAPH_STYLE,
                            body = DetailValue
                        }
                    }
                ]
            }
        end, [
            {<<"Name">>, SpaceName},
            {<<"Space ID">>, SpaceId}
        ])
    }.


providers_table_collapsed(ProvidersDetails) ->
    TableName = <<"Providers">>,
    NavigationBody = onepanel_gui_utils:expand_button(<<"Expand All">>, {message, expand_providers_table}),
    RenderRowFunction = fun provider_row_collapsed/2,
    table(ProvidersDetails, TableName, NavigationBody, RenderRowFunction).


providers_table_expanded(ProvidersDetails) ->
    TableName = <<"Providers">>,
    NavigationBody = onepanel_gui_utils:collapse_button(<<"Collapse All">>, {message, collapse_providers_table}),
    RenderRowFunction = fun provider_row_expanded/2,
    table(ProvidersDetails, TableName, NavigationBody, RenderRowFunction).


users_table_collapsed(UsersDetails) ->
    TableName = <<"Users">>,
    NavigationBody = onepanel_gui_utils:expand_button(<<"Expand All">>, {message, expand_users_table}),
    RenderRowFunction = fun user_row_collapsed/2,
    table(UsersDetails, TableName, NavigationBody, RenderRowFunction).


users_table_expanded(UsersDetails) ->
    TableName = <<"Users">>,
    NavigationBody = onepanel_gui_utils:collapse_button(<<"Collapse All">>, {message, collapse_users_table}),
    RenderRowFunction = fun user_row_expanded/2,
    table(UsersDetails, TableName, NavigationBody, RenderRowFunction).


table(Details, TableName, NavigationBody, RenderRowFunction) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = TableName
            },
            #th{
                style = ?NAVIGATION_COLUMN_STYLE,
                body = NavigationBody
            }
        ]
    },

    Rows = lists:foldl(fun({RowId, RowDetails}, RowsAcc) ->
        [#tr{
            id = RowId,
            cells = RenderRowFunction(RowId, RowDetails)
        } | RowsAcc]
    end, [], Details),

    [Header | Rows].


provider_row_collapsed(RowId, #{<<"id">> := ProviderId, <<"name">> := ProviderName} = ProviderDetails) ->
    NavigationBody = onepanel_gui_utils:expand_button({message, {expand_provider_row, RowId, ProviderDetails}}),
    row_collapsed(ProviderId, ProviderName, NavigationBody).


provider_row_expanded(RowId, #{<<"id">> := ProviderId, <<"name">> := ProviderName,
    <<"redirectionPoint">> := RedirectionPoint, <<"urls">> := URLs} = ProviderDetails) ->
    Details = [
        {<<"Name">>, #p{style = ?PARAGRAPH_STYLE, body = ProviderName}},
        {<<"Provider ID">>, #p{style = ?PARAGRAPH_STYLE, body = ProviderId}},
        {<<"URLs">>, #list{
            style = <<"list-style-type: none; margin: 0 auto;">>,
            body = lists:map(fun(URL) ->
                #li{body = #p{
                    style = ?PARAGRAPH_STYLE,
                    body = URL}
                }
            end, lists:sort(URLs))
        }},
        {<<"Redirection point">>, #p{style = ?PARAGRAPH_STYLE, body = RedirectionPoint}}
    ],
    NavigationBody = onepanel_gui_utils:collapse_button({message, {collapse_provider_row, RowId, ProviderDetails}}),
    row_expanded(Details, NavigationBody).


user_row_collapsed(RowId, #{<<"id">> := UserId, <<"name">> := UserName} = UserDetails) ->
    NavigationBody = onepanel_gui_utils:expand_button({message, {expand_user_row, RowId, UserDetails}}),
    row_collapsed(UserId, UserName, NavigationBody).


user_row_expanded(RowId, #{<<"id">> := UserId, <<"name">> := UserName} = UserDetails) ->
    Details = [
        {<<"Name">>, #p{style = ?PARAGRAPH_STYLE, body = UserName}},
        {<<"User ID">>, #p{style = ?PARAGRAPH_STYLE, body = UserId}}
    ],
    NavigationBody = onepanel_gui_utils:collapse_button({message, {collapse_user_row, RowId, UserDetails}}),
    row_expanded(Details, NavigationBody).


row_collapsed(Id, Name, NavigationBody) ->
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #p{
                style = ?PARAGRAPH_STYLE,
                body = <<"<b>", Name/binary, "</b> (", Id/binary, ")">>
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = NavigationBody
        }
    ].


row_expanded(Details, NavigationBody) ->
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = <<"border-width: 0; width: 100%; border-collapse: inherit;">>,
                body = lists:map(fun({DetailName, DetailBody}) ->
                    #tr{
                        cells = [
                            #td{
                                style = <<"border-width: 0; text-align: right; width: 10%; padding-left: 0; padding-right: 0;">>,
                                body = #label{
                                    style = <<"margin: 0 auto; cursor: auto;">>,
                                    class = <<"label label-large label-inverse">>,
                                    body = DetailName
                                }
                            },
                            #td{
                                style = <<"border-width: 0;  text-align: left; padding-left: 1em; width: 90%;">>,
                                body = DetailBody
                            }
                        ]
                    }
                end, Details)
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = NavigationBody
        }
    ].


%% ====================================================================
%% Events handling
%% ====================================================================

comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#{providers_details := ProvidersDetails, users_details := UsersDetails} = State) ->
    NewState = try
        receive
            render_tables ->
                gui_jq:update(<<"providers_table">>, providers_table_collapsed(ProvidersDetails)),
                gui_jq:fade_in(<<"providers_table">>, 500),
                gui_jq:update(<<"users_table">>, users_table_collapsed(UsersDetails)),
                gui_jq:fade_in(<<"users_table">>, 500),
                State;

            Event ->
                case Event of
                    collapse_providers_table ->
                        gui_jq:update(<<"providers_table">>, providers_table_collapsed(ProvidersDetails));
                    expand_providers_table ->
                        gui_jq:update(<<"providers_table">>, providers_table_expanded(ProvidersDetails));
                    {collapse_provider_row, RowId, ProviderDetails} ->
                        gui_jq:update(RowId, provider_row_collapsed(RowId, ProviderDetails));
                    {expand_provider_row, RowId, ProviderDetails} ->
                        gui_jq:update(RowId, provider_row_expanded(RowId, ProviderDetails));
                    collapse_users_table ->
                        gui_jq:update(<<"users_table">>, users_table_collapsed(UsersDetails));
                    expand_users_table ->
                        gui_jq:update(<<"users_table">>, users_table_expanded(UsersDetails));
                    {collapse_user_row, RowId, UserDetails} ->
                        gui_jq:update(RowId, user_row_collapsed(RowId, UserDetails));
                    {expand_user_row, RowId, UserDetails} ->
                        gui_jq:update(RowId, user_row_expanded(RowId, UserDetails));
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
        SpaceId = str_utils:to_binary(gui_ctx:url_param(<<"id">>)),

        GetDetailsFun = fun(Ids, Function, RowPrefix) ->
            lists:foldl(fun(Id, {Rows, It}) ->
                {ok, Details} = onepanel_gui_logic:Function(SpaceId, Id),
                {
                    [{<<RowPrefix/binary, (integer_to_binary(It + 1))/binary>>, Details} | Rows],
                    It + 1
                }
            end, {[], 0}, Ids)
        end,

        {ok, ProviderIds} = onepanel_gui_logic:get_providers(SpaceId),
        {ProvidersDetails, _} = GetDetailsFun(ProviderIds, get_provider_details, <<"provider_">>),
        {ok, UserIds} = onepanel_gui_logic:get_users(SpaceId),
        {UsersDetails, _} = GetDetailsFun(UserIds, get_user_details, <<"user_">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#{providers_details => ProvidersDetails, users_details => UsersDetails})
        end),
        put(comet, Pid),
        Pid ! render_tables
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch Space details.<br>Please try again later.">>)
    end;

event({message, Message}) ->
    get(comet) ! Message,
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.