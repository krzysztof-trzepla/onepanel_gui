%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros and records for
%% web pages.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_GUI_COMMON_HRL).
-define(ONEPANEL_GUI_COMMON_HRL, 1).

-include("pages.hrl").
-include("errors.hrl").
-include("registered_names.hrl").
-include_lib("n2o/include/wf.hrl").

%% Any custom element records should go here.

% FlatUI compliant radio button
-record(flatui_radio, {?ELEMENT_BASE(flatui_radio),
    label_id,
    label_class,
    label_style,
    label_title,
    autofocus,
    checked = false,
    disabled,
    form,
    name,
    required,
    value,
    postback,
    html_name
}).

% FlatUI compliant checkbox
-record(flatui_checkbox, {?ELEMENT_BASE(flatui_checkbox),
    label_id,
    label_class,
    label_style,
    label_title,
    autofocus,
    checked = false,
    disabled,
    form,
    name,
    required,
    value,
    postback
}).

-record(flatui_label, {?ELEMENT_BASE(flatui_label),
    for,
    form,
    postback
}).

%% Delay in miliseconds after which comet process will reload it's code
-define(COMET_PROCESS_RELOAD_DELAY, 5000).

%% Page content starting height
-define(SUBMENU_HEIGHT, 107).

-endif.
