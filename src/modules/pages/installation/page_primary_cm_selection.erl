%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select main Cluster Manager host.
%% @end
%% ===================================================================
-module(page_primary_cm_selection).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    try
        case gui_ctx:user_logged_in() of
            true ->
                case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION, ?PAGE_INSTALLATION) of
                    true ->
                        #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                    _ ->
                        #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
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
    <<"Primary CM selection">>.


body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Hosts selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION},
        {<<"Primary CM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 2: Primary Cluster Manager selection.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Cluster Manager</i> components control and organize work of other"
                " application components. However, it is not possible for more than one <i>Cluster Manager</i> component"
                " to run simultaneously. Therefore, it is required to select primary <i>Cluster Manager</i> component"
                " which will execute aforementioned tasks, while other <i>Cluster Manager</i> components will wait"
                " and in case of primary <i>Cluster Manager</i> breakdown take over its duties.">>
            },
            #panel{
                class = <<"btn-group">>,
                body = main_cm()
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"next_button">>, {postback, next}, false, <<"Next">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main).


main_cm() ->
    Host = onepanel_gui_ctx:get_main_cm_host(),
    [
        <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
        #button{
            class = <<"btn btn-inverse btn-small dropdown-toggle">>,
            data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
            body = [
                #span{
                    id = <<"cms_label">>,
                    style = <<"padding-right: 1em;">>,
                    class = <<"filter-option pull-left">>,
                    body = <<"Primary CM host: <b>", (http_utils:html_encode(
                        Host))/binary, "</b>">>
                },
                #span{
                    class = <<"caret pull-right">>
                }
            ]
        },
        #list{
            id = <<"cms_dropdown">>,
            class = <<"dropdown-menu dropdown-inverse">>,
            style = <<"overflow-y: auto; max-height: 20em;">>,
            body = cms_list(Host)
        }
    ].


cms_list(MainHost) ->
    Hosts = lists:reverse(onepanel_gui_ctx:get_hosts(managers)),
    {Body, _} = lists:foldl(fun(Host, {List, Id}) ->
        HostId = <<"cm_li_", (integer_to_binary(Id))/binary>>,
        {
            [#li{
                id = HostId,
                actions = gui_jq:postback_action(HostId, {set_main_cm, Host}),
                class = case Host of
                    MainHost -> <<"active">>;
                    _ -> <<"">>
                end,
                body = #link{
                    style = <<"text-align: left;">>,
                    body = http_utils:html_encode(Host)
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, Hosts),
    Body.


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    ok;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION);

event(next) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY);

event({set_main_cm, MainHost}) ->
    onepanel_gui_ctx:set_main_cm_host(MainHost),
    gui_jq:update(<<"cms_label">>, <<"Primary CM host: <b>",
        (http_utils:html_encode(MainHost))/binary, "</b>">>),
    gui_jq:update(<<"cms_dropdown">>, cms_list(MainHost));

event(terminate) ->
    ok.