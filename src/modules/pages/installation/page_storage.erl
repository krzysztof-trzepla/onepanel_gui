%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to add storage during software components installation.
%% @end
%% ===================================================================
-module(page_storage).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    try
        case gui_ctx:user_logged_in() of
            true ->
                case onepanel_gui_logic:get_hosts(workers) of
                    {error, not_found} ->
                        page_error:redirect_with_error(?SOFTWARE_NOT_INSTALLED_ERROR),
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
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end.


title() ->
    <<"Storage configuration">>.


body() ->
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, storage_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Storage configuration.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Worker</i> components save and retrieve user's data from network file system"
                " storages.">>
            },
            #panel{
                style = <<"display: inline-block">>,
                body = [
                    #panel{
                        class = <<"btn-group">>,
                        body = storage_type_dropdown()
                    },
                    ceph_storage_panel(),
                    posix_storage_panel(),
                    s3_storage_panel(),
                    swift_storage_panel()
                ]
            },
            #table{
                class = <<"table table-striped">>,
                style = <<"width: 50%; margin: 0 auto; margin-top: 3em;">>,
                body = #tbody{
                    id = <<"storage_paths_table">>,
                    style = <<"display: none;">>
                }
            }
        ]
    },
    onepanel_gui_utils:body(61, Header, Main).


storage_type_dropdown() ->
    [
        <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
        #button{
            class = <<"btn btn-inverse dropdown-toggle">>,
            data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
            body = [
                #span{
                    id = <<"storage_type_label">>,
                    style = <<"padding-right: 1em; min-width: 10em;">>,
                    class = <<"filter-option pull-left">>,
                    body = <<"Storage type: <b>S3</b>">>
                },
                #span{
                    class = <<"caret pull-right">>
                }
            ]
        },
        #list{
            id = <<"storage_type_dropdown">>,
            class = <<"dropdown-menu dropdown-inverse">>,
            style = <<"overflow-y: auto; max-height: 20em;">>,
            body = storage_type_list(<<"S3">>, [<<"Swift">>, <<"Posix">>, <<"Ceph">>, <<"S3">>])
        }
    ].


storage_type_list(StorageType, StorageTypes) ->
    {Body, _} = lists:foldl(fun(Type, {List, Id}) ->
        TypeId = <<"storage_type_li_", (integer_to_binary(Id))/binary>>,
        {
            [#li{
                id = TypeId,
                actions = gui_jq:postback_action(TypeId, {set_storage_type, Type, StorageTypes}),
                class = case Type of
                    StorageType -> <<"active">>;
                    _ -> <<"">>
                end,
                body = #link{
                    style = <<"text-align: left;">>,
                    body = http_utils:html_encode(Type)
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, StorageTypes),
    Body.


ceph_storage_panel() ->
    #panel{
        id = <<"ceph_storage">>,
        style = <<"margin-top: 0.75em; display: none;">>,
        actions = gui_jq:form_submit_action(<<"ceph_submit">>,
            ceph_submit, [<<"ceph_storage_name">>, <<"ceph_username">>,
                <<"ceph_key">>, <<"ceph_mon_host">>,
                <<"ceph_cluster_name">>, <<"ceph_pool_name">>,
                <<"ceph_timeout">>]),
        body = [
            #textbox{
                id = <<"ceph_storage_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Storage name">>
            },
            #textbox{
                id = <<"ceph_username">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Admin username">>
            },
            #password{
                id = <<"ceph_key">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Admin key">>
            },
            #textbox{
                id = <<"ceph_mon_host">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Monitor hostname">>
            },
            #textbox{
                id = <<"ceph_cluster_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Cluster name">>
            },
            #textbox{
                id = <<"ceph_pool_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Pool name">>
            },
            #textbox{
                id = <<"ceph_timeout">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Timeout [ms] (optional)">>
            },
            #flatui_checkbox{
                label_id = <<"ceph_insecure_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {insecure_toggled, ceph},
                body = <<"Insecure">>
            },
            #flatui_checkbox{
                label_id = <<"readonly_storage_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {readonly_toggled, ceph},
                body = <<"Readonly">>
            },
            #button{
                id = <<"ceph_submit">>,
                class = <<"btn btn-inverse">>,
                style = <<"width: 10em;">>,
                body = <<"Add">>
            }
        ]
    }.


posix_storage_panel() ->
    #panel{
        id = <<"posix_storage">>,
        style = <<"margin-top: 0.75em; display: none;">>,
        actions = gui_jq:form_submit_action(<<"posix_submit">>,
            posix_submit, [<<"posix_storage_name">>, <<"posix_mount_point">>,
                <<"posix_timeout">>]),
        body = [
            #textbox{
                id = <<"posix_storage_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Storage name">>
            },
            #textbox{
                id = <<"posix_mount_point">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Mount point">>
            },
            #textbox{
                id = <<"posix_timeout">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Timeout [ms] (optional)">>
            },
            #button{
                id = <<"posix_submit">>,
                class = <<"btn btn-inverse">>,
                style = <<"width: 10em;">>,
                body = <<"Add">>
            },
            #flatui_checkbox{
                label_id = <<"readonly_storage_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {readonly_toggled, posix},
                body = <<"Readonly">>
            }
        ]
    }.


s3_storage_panel() ->
    #panel{
        id = <<"s3_storage">>,
        style = <<"margin-top: 0.75em;">>,
        actions = gui_jq:form_submit_action(<<"s3_submit">>,
            s3_submit, [<<"s3_storage_name">>, <<"s3_access_key">>,
                <<"s3_secret_key">>, <<"s3_hostname">>, <<"s3_bucket_name">>,
                <<"s3_block_size">>, <<"s3_timeout">>]),
        body = [
            #textbox{
                id = <<"s3_storage_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Storage name">>
            },
            #textbox{
                id = <<"s3_access_key">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Admin access key">>
            },
            #password{
                id = <<"s3_secret_key">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Admin secret key">>
            },
            #textbox{
                id = <<"s3_hostname">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Hostname">>,
                value = <<"s3.amazonaws.com">>
            },
            #textbox{
                id = <<"s3_bucket_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Bucket name">>
            },
            #textbox{
                id = <<"s3_block_size">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Block size [bytes] (optional)">>
            },
            #textbox{
                id = <<"s3_timeout">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Timeout [ms] (optional)">>
            },
            #flatui_checkbox{
                label_id = <<"s3_insecure_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {insecure_toggled, s3},
                body = <<"Insecure">>
            },
            #flatui_checkbox{
                label_id = <<"readonly_storage_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {readonly_toggled, s3},
                body = <<"Readonly">>
            },
            #button{
                id = <<"s3_submit">>,
                class = <<"btn btn-inverse">>,
                style = <<"width: 10em;">>,
                body = <<"Add">>
            }
        ]
    }.


swift_storage_panel() ->
    #panel{
        id = <<"swift_storage">>,
        style = <<"margin-top: 0.75em; display: none;">>,
        actions = gui_jq:form_submit_action(<<"swift_submit">>,
            swift_submit, [<<"swift_storage_name">>, <<"swift_auth_url">>,
                <<"swift_username">>, <<"swift_password">>,
                <<"swift_container_name">>, <<"swift_tenant_name">>,
                <<"swift_block_size">>, <<"swift_timeout">>]),
        body = [
            #textbox{
                id = <<"swift_storage_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Storage name">>
            },
            #textbox{
                id = <<"swift_username">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Admin username">>
            },
            #password{
                id = <<"swift_password">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Admin password">>
            },
            #textbox{
                id = <<"swift_auth_url">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Auth URL">>
            },
            #textbox{
                id = <<"swift_container_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Container name">>
            },
            #textbox{
                id = <<"swift_tenant_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Tenant name">>
            },
            #textbox{
                id = <<"swift_block_size">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Block size [bytes] (optional)">>
            },
            #textbox{
                id = <<"swift_timeout">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Timeout [ms] (optional)">>
            },
            #flatui_checkbox{
                label_id = <<"swift_insecure_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {insecure_toggled, swift},
                body = <<"Insecure">>
            },
            #flatui_checkbox{
                label_id = <<"readonly_storage_checkbox">>,
                label_style = <<"width: 20px;">>,
                label_class = <<"checkbox">>,
                delegate = ?MODULE,
                postback = {readonly_toggled, swift},
                body = <<"Readonly">>
            },
            #button{
                id = <<"swift_submit">>,
                class = <<"btn btn-inverse">>,
                style = <<"width: 10em;">>,
                body = <<"Add">>
            }
        ]
    }.


get_opt_param(Name, DisplayName, Params) ->
    case maps:find(Name, Params) of
        {ok, Value} -> {DisplayName, Value};
        error -> {DisplayName, <<"default">>}
    end.


storage_table(Storages) ->
    Header = #tr{
        cells = lists:map(fun(Name) ->
            #th{
                style = <<"text-align: center;">>,
                body = Name
            }
        end, [<<"Storage name">>, <<"Storage type">>, <<"Storage properties">>])
    },
    Rows = lists:map(fun
        ({Name, #{<<"monitorHostname">> := MonHost, <<"clusterName">> := ClusterName,
            <<"poolName">> := PoolName} = Params}) ->
            storage_table_row(Name, <<"Ceph">>, [{<<"Monitor host">>, MonHost},
                {<<"Cluster name">>, ClusterName}, {<<"Pool name">>, PoolName},
                get_opt_param(<<"timeout">>, <<"Timeout [ms]">>, Params)]);
        ({Name, #{<<"mountPoint">> := Path} = Params}) ->
            storage_table_row(Name, <<"Posix">>, [{<<"Mount point">>, Path},
                get_opt_param(<<"timeout">>, <<"Timeout [ms]">>, Params)]);
        ({Name, #{<<"hostname">> := Hostname, <<"bucketName">> := BucketName} = Params}) ->
            storage_table_row(Name, <<"S3">>, [{<<"Hostname">>, Hostname},
                {<<"Bucket name">>, BucketName},
                get_opt_param(<<"timeout">>, <<"Timeout [ms]">>, Params),
                get_opt_param(<<"blockSize">>, <<"Block size [bytes]">>, Params)]);
        ({Name, #{<<"authUrl">> := AuthUrl, <<"containerName">> := ContainerName,
            <<"tenantName">> := TenantName} = Params}) ->
            storage_table_row(Name, <<"Swift">>, [{<<"Auth URL">>, AuthUrl},
                {<<"Container name">>, ContainerName}, {<<"Tenant name">>, TenantName},
                get_opt_param(<<"timeout">>, <<"Timeout [ms]">>, Params),
                get_opt_param(<<"blockSize">>, <<"Block size [bytes]">>, Params)])
    end, Storages),
    [Header | Rows].


storage_table_row(Name, Type, Params) ->
    #tr{
        cells = [
            #th{
                style = <<"text-align: center; font-weight: normal;">>,
                body = #p{
                    style = <<"font-size: inherit; margin: 0px;">>,
                    body = Name
                }
            },
            #th{
                style = <<"text-align: center; font-weight: normal;">>,
                body = #p{
                    style = <<"font-size: inherit; margin: 0px;">>,
                    body = Type
                }
            },
            #th{
                style = <<"text-align: center; font-weight: normal;">>,
                body = lists:map(fun({Key, Value}) ->
                    EKey = http_utils:html_encode(Key),
                    EValue = http_utils:html_encode(Value),
                    #p{
                        style = <<"font-size: inherit; margin: 0px;">>,
                        body = <<"<b>", EKey/binary, "</b>: ", EValue/binary>>
                    }
                end, Params)
            }
        ]
    }.


reset_panel() ->
    lists:foreach(fun(Id) ->
        gui_jq:remove_class(Id, <<"checked">>)
    end, [
        <<"ceph_insecure_checkbox">>,
        <<"s3_insecure_checkbox">>,
        <<"swift_insecure_checkbox">>
    ]),
    lists:foreach(fun
        ({Id, Value}) -> gui_jq:set_value(Id, Value);
        (Id) -> gui_jq:set_value(Id, <<"''">>)
    end, [<<"ceph_storage_name">>, <<"ceph_username">>, <<"ceph_key">>,
        <<"ceph_mon_host">>, <<"ceph_cluster_name">>, <<"ceph_pool_name">>,
        <<"ceph_timeout">>, <<"posix_storage_name">>, <<"posix_mount_point">>,
        <<"posix_timeout">>, <<"s3_storage_name">>, <<"s3_access_key">>,
        <<"s3_secret_key">>, {<<"s3_hostname">>, <<"s3.amazonaws.com">>},
        <<"s3_bucket_name">>, <<"s3_bucket_size">>, <<"s3_timeout">>,
        <<"swift_storage_name">>, <<"swift_username">>, <<"swift_password">>,
        <<"swift_auth_url">>, <<"swift_container_name">>,
        <<"swift_tenant_name">>, <<"swift_block_size">>, <<"swift_timeout">>]).


strip(Text) ->
    list_to_binary(string:strip(binary_to_list(Text))).

prepare_opt_args([], Acc) ->
    Acc;
prepare_opt_args([{_Name, <<>>} | Args], Acc) ->
    prepare_opt_args(Args, Acc);
prepare_opt_args([{Name, Value} | Args], Acc) ->
    prepare_opt_args(Args, [{Name, erlang:binary_to_integer(Value)} | Acc]).

%% ====================================================================
%% Events handling
%% ====================================================================


comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#{storage_type := StorageType} = State) ->
    NewState = try
        receive
            render_storages_table ->
                {ok, Storages} = onepanel_gui_logic:get_storages(),
                gui_jq:update(<<"storage_paths_table">>, storage_table(Storages)),
                gui_jq:fade_in(<<"storage_paths_table">>, 500),
                gui_jq:focus(<<"s3_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"s3_bucket_name">>, <<"s3_submit">>),
                State;

            {set_storage_type, StorageType} ->
                State;

            {set_storage_type, <<"S3">> = SType} ->
                gui_jq:show(<<"s3_storage">>),
                gui_jq:hide(<<"posix_storage">>),
                gui_jq:hide(<<"ceph_storage">>),
                gui_jq:hide(<<"swift_storage">>),
                reset_panel(),
                gui_jq:focus(<<"s3_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"s3_timeout">>, <<"s3_submit">>),
                #{storage_type => SType};

            {set_storage_type, <<"Ceph">> = SType} ->
                gui_jq:show(<<"ceph_storage">>),
                gui_jq:hide(<<"posix_storage">>),
                gui_jq:hide(<<"s3_storage">>),
                gui_jq:hide(<<"swift_storage">>),
                reset_panel(),
                gui_jq:focus(<<"ceph_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"ceph_timeout">>, <<"ceph_submit">>),
                #{storage_type => SType};

            {set_storage_type, <<"Posix">> = SType} ->
                gui_jq:show(<<"posix_storage">>),
                gui_jq:hide(<<"ceph_storage">>),
                gui_jq:hide(<<"s3_storage">>),
                gui_jq:hide(<<"swift_storage">>),
                reset_panel(),
                gui_jq:focus(<<"posix_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"posix_timeout">>, <<"posix_submit">>),
                #{storage_type => SType};

            {set_storage_type, <<"Swift">> = SType} ->
                gui_jq:show(<<"swift_storage">>),
                gui_jq:hide(<<"posix_storage">>),
                gui_jq:hide(<<"ceph_storage">>),
                gui_jq:hide(<<"s3_storage">>),
                reset_panel(),
                gui_jq:focus(<<"swift_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"swift_timeout">>, <<"swift_submit">>),
                #{storage_type => SType};

            {insecure_toggled, Storage} ->
                Insecure = maps:get(Storage, State, false),
                maps:put(Storage, not Insecure, State);

            {readonly_toggled, Storage} ->
                Readonly = maps:get(Storage, State, false),
                maps:put(Storage, not Readonly, State);

            {ceph_submit, <<>>, _, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide admin username.">>),
                State;

            {ceph_submit, _, <<>>, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide admin key.">>),
                State;

            {ceph_submit, _, _, <<>>, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide storage name.">>),
                State;

            {ceph_submit, _, _, _, <<>>, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide monitor host.">>),
                State;

            {ceph_submit, _, _, _, _, <<>>, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide cluster name.">>),
                State;

            {ceph_submit, _, _, _, _, _, <<>>, _} ->
                onepanel_gui_utils:message(error, <<"Please provide pool name.">>),
                State;

            {ceph_submit, Username, Key, StorageName, MonHost, ClusterName, PoolName, Timeout} ->
                OptArgs = prepare_opt_args([{timeout, Timeout}], []),
                case onepanel_gui_logic:add_storage([{StorageName, [{type, <<"ceph">>},
                    {monitorHostname, MonHost}, {clusterName, ClusterName},
                    {poolName, PoolName}, {username, Username}, {key, Key},
                    {insecure, maps:get(ceph, State, false)} | OptArgs]}]) of
                    ok ->
                        onepanel_gui_utils:message(success, <<"Storage successfully added.">>),
                        reset_panel(),
                        gui_jq:focus(<<"ceph_storage_name">>),
                        self() ! render_storages_table;
                    {error, _} ->
                        onepanel_gui_utils:message(error, <<"There has been an error while adding storage. Please try again later.">>)
                end,
                State;

            {posix_submit, <<>>, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide storage name.">>),
                State;

            {posix_submit, _, <<>>, _} ->
                onepanel_gui_utils:message(error, <<"Please provide mount point.">>),
                State;

            {posix_submit, StorageName, MountPoint, Timeout} ->
                OptArgs = prepare_opt_args([{timeout, Timeout}], []),
                case onepanel_gui_logic:add_storage([{StorageName,
                    [{type, <<"posix">>}, {mountPoint, MountPoint} | OptArgs]}]) of
                    ok ->
                        onepanel_gui_utils:message(success, <<"Storage successfully added.">>),
                        reset_panel(),
                        gui_jq:focus(<<"posix_storage_name">>),
                        self() ! render_storages_table;
                    {error, _} ->
                        onepanel_gui_utils:message(error, <<"There has been an error while adding storage. Please try again later.">>)
                end,
                State;

            {s3_submit, <<>>, _, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide storage name.">>),
                State;

            {s3_submit, _, <<>>, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide admin access key.">>),
                State;

            {s3_submit, _, _, <<>>, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide admin secret key.">>),
                State;

            {s3_submit, _, _, _, <<>>, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide hostname.">>),
                State;

            {s3_submit, _, _, _, _, <<>>, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide bucket name.">>),
                State;

            {s3_submit, StorageName, AccessKey, SecretKey, Hostname, BucketName, BlockSize, Timeout} ->
                OptArgs = prepare_opt_args([{timeout, Timeout}, {blockSize, BlockSize}], []),
                case onepanel_gui_logic:add_storage([{StorageName, [{type, <<"s3">>},
                    {hostname, Hostname}, {bucketName, BucketName}, {accessKey, AccessKey},
                    {secretKey, SecretKey}, {insecure, maps:get(s3, State, false)} | OptArgs]}]) of
                    ok ->
                        onepanel_gui_utils:message(success, <<"Storage successfully added.">>),
                        reset_panel(),
                        gui_jq:focus(<<"s3_storage_name">>),
                        self() ! render_storages_table;
                    {error, _} ->
                        onepanel_gui_utils:message(error, <<"There has been an error while adding storage. Please try again later.">>)
                end,
                State;

            {swift_submit, <<>>, _, _, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide storage name.">>),
                State;

            {swift_submit, _, <<>>, _, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide admin username.">>),
                State;

            {swift_submit, _, _, <<>>, _, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide admin password.">>),
                State;

            {swift_submit, _, _, _, <<>>, _, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide auth URL.">>),
                State;

            {swift_submit, _, _, _, _, <<>>, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide container name.">>),
                State;

            {swift_submit, _, _, _, _, _, <<>>, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide tenant name.">>),
                State;

            {swift_submit, StorageName, Username, Password, AuthUrl, ContainerName, TenantName, BlockSize, Timeout} ->
                OptArgs = prepare_opt_args([{timeout, Timeout}, {blockSize, BlockSize}], []),
                case onepanel_gui_logic:add_storage([{StorageName, [{type, <<"swift">>}, {username, Username},
                    {password, Password}, {authUrl, AuthUrl}, {containerName, ContainerName},
                    {tenantName, TenantName}, {insecure, maps:get(swift, State, false)} | OptArgs]}]) of
                    ok ->
                        onepanel_gui_utils:message(success, <<"Storage successfully added.">>),
                        reset_panel(),
                        gui_jq:focus(<<"swift_storage_name">>),
                        self() ! render_storages_table;
                    {error, _} ->
                        onepanel_gui_utils:message(error, <<"There has been an error while adding storage. Please try again later.">>)
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
        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#{storage_type => undefined})
        end),
        put(comet, Pid),
        Pid ! render_storages_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({set_storage_type, Type, StorageTypes}) ->
    gui_jq:show(<<"main_spinner">>),
    gui_jq:update(<<"storage_type_label">>, <<"Storage type: <b>", Type/binary, "</b>">>),
    gui_jq:update(<<"storage_type_dropdown">>, storage_type_list(Type, StorageTypes)),
    get(comet) ! {set_storage_type, Type};

event(ceph_submit) ->
    gui_jq:show(<<"main_spinner">>),
    Username = gui_ctx:postback_param(<<"ceph_username">>),
    Key = gui_ctx:postback_param(<<"ceph_key">>),
    StorageName = gui_ctx:postback_param(<<"ceph_storage_name">>),
    MonHost = gui_ctx:postback_param(<<"ceph_mon_host">>),
    ClusterName = gui_ctx:postback_param(<<"ceph_cluster_name">>),
    PoolName = gui_ctx:postback_param(<<"ceph_pool_name">>),
    Timeout = gui_ctx:postback_param(<<"ceph_timeout">>),
    get(comet) ! {ceph_submit, strip(Username), strip(Key), strip(StorageName),
        strip(MonHost), strip(ClusterName), strip(PoolName), strip(Timeout)};

event(posix_submit) ->
    gui_jq:show(<<"main_spinner">>),
    StorageName = gui_ctx:postback_param(<<"posix_storage_name">>),
    MountPoint = gui_ctx:postback_param(<<"posix_mount_point">>),
    Timeout = gui_ctx:postback_param(<<"posix_timeout">>),
    get(comet) ! {posix_submit, strip(StorageName), strip(MountPoint), strip(Timeout)};

event(s3_submit) ->
    gui_jq:show(<<"main_spinner">>),
    StorageName = gui_ctx:postback_param(<<"s3_storage_name">>),
    AccessKey = gui_ctx:postback_param(<<"s3_access_key">>),
    SecretKey = gui_ctx:postback_param(<<"s3_secret_key">>),
    Hostname = gui_ctx:postback_param(<<"s3_hostname">>),
    BucketName = gui_ctx:postback_param(<<"s3_bucket_name">>),
    BlockSize = gui_ctx:postback_param(<<"s3_block_size">>),
    Timeout = gui_ctx:postback_param(<<"s3_timeout">>),
    get(comet) ! {s3_submit, strip(StorageName), strip(AccessKey),
        strip(SecretKey), strip(Hostname), strip(BucketName), strip(BlockSize),
        strip(Timeout)};

event(swift_submit) ->
    gui_jq:show(<<"main_spinner">>),
    StorageName = gui_ctx:postback_param(<<"swift_storage_name">>),
    Username = gui_ctx:postback_param(<<"swift_username">>),
    Password = gui_ctx:postback_param(<<"swift_password">>),
    AuthUrl = gui_ctx:postback_param(<<"swift_auth_url">>),
    ContainerName = gui_ctx:postback_param(<<"swift_container_name">>),
    TenantName = gui_ctx:postback_param(<<"swift_tenant_name">>),
    BlockSize = gui_ctx:postback_param(<<"swift_block_size">>),
    Timeout = gui_ctx:postback_param(<<"swift_timeout">>),
    get(comet) ! {swift_submit, strip(StorageName), strip(Username), strip(Password),
        strip(AuthUrl), strip(ContainerName), strip(TenantName), strip(BlockSize),
        strip(Timeout)};

event({insecure_toggled, Storage}) ->
    get(comet) ! {insecure_toggled, Storage};

event({readonly_toggled, Storage}) ->
    get(comet) ! {readonly_toggled, Storage};

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.