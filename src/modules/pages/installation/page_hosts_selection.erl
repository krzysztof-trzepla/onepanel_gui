%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select hosts during software components installation.
%% @end
%% ===================================================================
-module(page_hosts_selection).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION, ?PAGE_INSTALLATION) of
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
    <<"Hosts selection">>.


body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Hosts selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 1: Hosts selection.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"The table below presents a list of hosts where software package installation has been detected.<br>"
                " In order to configure application please distribute software components over available hosts by selecting"
                " checkboxes.">>
            },
            #table{
                id = <<"hosts_table">>,
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>
            },
            onepanel_gui_utils:nav_buttons([{<<"next_button">>, {postback, {message, next}}, true, <<"Next">>}])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main).


hosts_table() ->
    Hosts = onepanel_gui_ctx:get_hosts(discovered),
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,

    Header = #tr{
        cells = lists:map(fun(ColumnName) ->
            #th{
                body = ColumnName,
                style = ColumnStyle
            }
        end, [<<"Host">>, <<"Database">>, <<"Cluster Manager">>, <<"Cluster Worker">>])
    },

    Rows = lists:map(fun({Host, Id}) ->
        HostId = integer_to_binary(Id),
        #tr{
            cells = [
                #td{
                    body = <<"<b>", (http_utils:html_encode(Host))/binary, "</b>">>,
                    style = ColumnStyle
                } | lists:map(fun({Prefix, Type}) ->
                    flatui_checkbox:init_checkbox(<<Prefix/binary, "checkbox_", HostId/binary>>),
                    #td{
                        style = ColumnStyle,
                        body = #flatui_checkbox{
                            label_id = <<Prefix/binary, "label_", HostId/binary>>,
                            label_style = <<"width: 20px; margin: 0 auto;">>,
                            label_class = <<"checkbox no-label">>,
                            id = <<Prefix/binary, "checkbox_", HostId/binary>>,
                            checked = onepanel_gui_ctx:host_member(Type, Host),
                            disabled = onepanel_gui_ctx:is_configured(),
                            delegate = ?MODULE,
                            postback = {message, {toggled, Type, Host}}
                        }
                    }
                end, [
                    {<<"db_">>, databases},
                    {<<"cm_">>, managers},
                    {<<"worker_">>, workers}
                ])
            ]
        }
    end, lists:zip(lists:sort(Hosts), tl(lists:seq(0, length(Hosts))))),

    [Header | Rows].


%% ====================================================================
%% Events handling
%% ====================================================================


comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(_) ->
    NewState = try
        receive
            render_hosts_table ->
                gui_jq:update(<<"hosts_table">>, hosts_table()),
                gui_jq:fade_in(<<"hosts_table">>, 500);

            next ->
                Next = lists:foldl(fun
                    (_, false) -> false;
                    ({Name, Type}, true) ->
                        case onepanel_gui_ctx:get_hosts(Type) of
                            [_ | _] -> true;
                            [] ->
                                onepanel_gui_utils:message(error, <<"Please select "
                                "at least one '", Name/binary, "' component.">>),
                                false
                        end
                end, true, [
                    {<<"Database">>, databases},
                    {<<"Cluster Manager">>, managers},
                    {<<"Cluster Worker">>, workers}
                ]),
                case Next of
                    true ->
                        onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION);
                    false ->
                        ok
                end;

            {toggled, Type, Host} ->
                onepanel_gui_ctx:host_toggle(Type, Host)

        after ?COMET_PROCESS_RELOAD_DELAY ->
            ok
        end
    catch Error:Reason ->
        ?error_stacktrace("Comet process exception: ~p:~p", [Error, Reason]),
        onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
        {error, Reason}
    end,
    gui_jq:wire(<<"$('#main_spinner').delay(300).hide(0);">>, false),
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


event(init) ->
    try
        {ok, DHosts} = onepanel_gui_logic:get_hosts(discovered),
        onepanel_gui_ctx:set_hosts(discovered, DHosts),

        onepanel_gui_ctx:set_configured(false),
        lists:foreach(fun(Type) ->
            case {onepanel_gui_logic:get_hosts(Type), DHosts} of
                {{error, not_found}, [Host]} ->
                    onepanel_gui_ctx:set_hosts(Type, [Host]);
                {{error, not_found}, _} ->
                    Hosts = onepanel_gui_ctx:get_hosts(Type),
                    onepanel_gui_ctx:set_hosts(Type, Hosts);
                {{ok, Hosts}, _} ->
                    onepanel_gui_ctx:set_hosts(Type, Hosts),
                    onepanel_gui_ctx:set_configured(true)
            end
        end, [databases, managers, workers]),

        case onepanel_gui_ctx:is_configured() of
            true -> ok;
            false -> gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>)
        end,

        gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),

        {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#{}) end),
        put(comet, Pid),
        Pid ! render_hosts_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({message, Message}) ->
    get(comet) ! Message;

event(recheck) ->
    get(comet) ! render_hosts_table;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.