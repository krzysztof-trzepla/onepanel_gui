%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains n2o website code.
%% This page contains the privacy policy.
%% @end
%% ===================================================================
-module(page_privacy_policy).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

-define(PRIVACY_POLICY_FILE, "PRIVACY_POLICY.html").

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


title() -> <<"Privacy policy">>.


body() ->
    #panel{
        style = <<"padding: 2em 5em;">>,
        body = [
            #h3{
                style = <<"margin-bottom: 30px;">>,
                body = <<"Privacy policy">>
            },
            #panel{
                body = privacy_policy_file()
            },
            #link{
                class = <<"btn btn-inverse btn-wide">>,
                style = <<"float: right; margin: 3em 0 1.5em;">>,
                url = ?PAGE_ROOT,
                body =
                <<"Main page">>
            }
        ]
    }.


privacy_policy_file() ->
    GuiStatic = filename:join(code:priv_dir(?APP_NAME), "gui_static"),
    PrivacyPolicyPath = filename:join(GuiStatic, ?PRIVACY_POLICY_FILE),
    case file:read_file(PrivacyPolicyPath) of
        {ok, File} ->
            File;
        {error, Reason} ->
            ?error("Cannot get privacy policy file ~s: ~p", [PrivacyPolicyPath, Reason]),
            <<"">>
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    ok;

event(terminate) ->
    ok.