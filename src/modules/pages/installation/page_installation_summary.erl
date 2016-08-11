%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page displays installation summary and starts installer process.
%% @end
%% ===================================================================
-module(page_installation_summary).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY, ?PAGE_INSTALLATION) of
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
    <<"Installation summary">>.


body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Hosts selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION},
        {<<"Primary CM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION},
        {<<"Installation summary">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 3: Installation summary.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Current application configuration is summarised in the table below.">>
            },
            #table{
                class = <<"table table-striped">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = #tbody{
                    id = <<"summary_table">>,
                    style = <<"display: none;">>
                }
            },
            #panel{
                id = <<"progress">>,
                style = <<"text-align: left; margin-top: 3em; width: 50%; margin: 0 auto; margin-top: 3em; display: none;">>,
                body = [
                    #p{
                        id = <<"progress_text">>,
                        style = <<"font-weight: 300;">>,
                        body = <<"">>
                    },
                    #panel{
                        class = <<"progress">>,
                        body = #panel{
                            id = <<"bar">>,
                            class = <<"bar">>,
                            style = <<"width: 0%;">>
                        }
                    }
                ]
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"install_button">>, {postback, install}, false, <<"Install">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main).


summary_table() ->
    lists:map(fun({Id, Description, Details}) ->
        #tr{
            id = Id,
            cells = [
                #th{
                    style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                    body = #p{
                        style = <<"text-align: center; margin-bottom: 0;">>,
                        body = Description
                    }
                },
                #th{
                    style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                    body = Details
                }
            ]
        }
    end, [
        {<<"summary_main_cm">>, <<"Primary <i>Cluster Manager</i> host">>, format([onepanel_gui_ctx:get_main_cm_host()])},
        {<<"summary_dbs">>, <<"<i>Database</i> hosts">>, format(onepanel_gui_ctx:get_hosts(databases))},
        {<<"summary_cms">>, <<"<i>Cluster Manager</i> hosts">>, format(onepanel_gui_ctx:get_hosts(managers))},
        {<<"summary_workers">>, <<"<i>Cluster Worker</i> hosts">>, format(onepanel_gui_ctx:get_hosts(workers))}
    ]).


format([]) ->
    #p{
        body = <<"-">>,
        style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>
    };
format(Hosts) ->
    lists:map(fun(Host) ->
        #p{
            body = http_utils:html_encode(Host),
            style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>
        }
    end, Hosts).


%% ====================================================================
%% Events handling
%% ====================================================================

comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(_) ->
    NewState = try
        receive
            render_summary_table ->
                gui_jq:update(<<"summary_table">>, summary_table()),
                gui_jq:fade_in(<<"summary_table">>, 500);

            install ->
                MainCmHost = onepanel_gui_ctx:get_main_cm_host(),
                CmHosts = onepanel_gui_ctx:get_hosts(managers),
                WrHosts = onepanel_gui_ctx:get_hosts(workers),
                DbHosts = onepanel_gui_ctx:get_hosts(databases),
                {ok, <<"/api/v3/onepanel", Endpoint/binary>>} =
                    onepanel_gui_logic:configure(MainCmHost, CmHosts, WrHosts, DbHosts),
                self() ! {init, Endpoint};

            {init, Endpoint} ->
                gui_jq:hide(<<"message">>),
                gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"disabled">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
                gui_jq:update(<<"progress_text">>, <<"<b>Stage: </b><i>initializing</i>">>),
                gui_jq:set_width(<<"bar">>, <<"0%">>),
                gui_jq:show(<<"progress">>),
                erlang:send_after(timer:seconds(1), self(), {progress, 1, Endpoint});

            {progress, Width, Endpoint} ->
                BWidth = <<(erlang:integer_to_binary(erlang:min(Width, 99)))/binary, "%">>,
                gui_jq:set_width(<<"bar">>, BWidth),
                case onepanel_gui_logic:get_progress(Endpoint) of
                    ok -> self() ! finish;
                    running -> erlang:send_after(timer:seconds(1), self(),
                        {progress, Width + 1, Endpoint});
                    {running, Step} ->
                        NewStep = binary:replace(Step, <<"_">>, <<" ">>, [global]),
                        [Module, Function] = binary:split(NewStep, <<":">>),
                        gui_jq:update(<<"progress_text">>, <<"<b>Stage: </b>",
                            Module/binary, " <i>", Function/binary, "</i>">>),
                        erlang:send_after(timer:seconds(1), self(),
                            {progress, Width + 1, Endpoint});
                    {error, Reason} ->
                        onepanel_gui_utils:message(error, Reason),
                        gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"">>),
                        gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                        gui_jq:hide(<<"progress">>)
                end;

            finish ->
                gui_jq:update(<<"progress_text">>, <<"">>),
                gui_jq:set_width(<<"bar">>, <<"100%">>),
                onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUCCESS);

            {error, Text} ->
                ?info("Received: ~p", [Text]),
                onepanel_gui_utils:message(error, Text),
                gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                gui_jq:hide(<<"progress">>);

            Ala ->
                ?info("Received: Ala ~p", [Ala])

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
    gui_jq:bind_key_to_click(<<"13">>, <<"install_button">>),
    try
        {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#{}) end),
        put(comet, Pid),
        Pid ! render_summary_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION);

event(install) ->
    get(comet) ! install;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.