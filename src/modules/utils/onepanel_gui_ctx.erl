%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains logic functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================
-module(onepanel_gui_ctx).

-include("registered_names.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([add_host/2, remove_host/2, set_hosts/2, get_hosts/1, host_member/2,
    host_toggle/2, set_main_cm_host/1, get_main_cm_host/0]).
-export([is_configured/0, set_configured/1]).

%% ====================================================================
%% API functions
%% ====================================================================

add_host(Type, Host) ->
    case gui_ctx:get(Type) of
        undefined -> gui_ctx:put(Type, [Host]);
        Hosts -> gui_ctx:put(Type, [Host | lists:delete(Host, Hosts)])
    end.


remove_host(Type, Host) ->
    case gui_ctx:get(Type) of
        undefined -> ok;
        Hosts -> gui_ctx:put(Type, lists:delete(Host, Hosts))
    end.

set_hosts(Type, Hosts) ->
    gui_ctx:put(Type, Hosts).


get_hosts(Type) ->
    case gui_ctx:get(Type) of
        undefined -> [];
        Hosts -> lists:sort(Hosts)
    end.


host_member(Type, Host) ->
    lists:member(Host, get_hosts(Type)).


host_toggle(Type, Host) ->
    case host_member(Type, Host) of
        true -> remove_host(Type, Host);
        false -> add_host(Type, Host)
    end.


set_main_cm_host(Host) ->
    gui_ctx:put(main_cm_host, Host).


get_main_cm_host() ->
    MainHost = case gui_ctx:get(main_cm_host) of
        undefined -> hd(get_hosts(managers));
        Host -> case host_member(managers, Host) of
            true -> Host;
            false -> hd(get_hosts(managers))
        end
    end,
    set_main_cm_host(MainHost),
    MainHost.


set_configured(Value) ->
    gui_ctx:put(configured, Value).


is_configured() ->
    gui_ctx:get(configured) == true.