%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page contains information about the project, licence and contact for support.
%% @end
%% ===================================================================
-module(page_about).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

-define(CONTACT_EMAIL, "support@onedata.org").

%% ====================================================================
%% API functions
%% ====================================================================

main() ->
    case gui_ctx:user_logged_in() of
        true ->
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
        _ ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


title() ->
    <<"About">>.


body() ->
    Header = onepanel_gui_utils_adapter:top_menu(about_tab),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"About">>
            },
            about_table()
        ]
    },
    onepanel_gui_utils:body(Header, Main).


about_table() ->
    DescriptionStyle = <<"border-width: 0; vertical-align: top; text-align: right; padding: 1em 1em;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: auto;">>,
        body = lists:map(fun({DescriptionBody, MainBody}) ->
            #tr{
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            style = <<"cursor: auto;">>,
                            body = DescriptionBody
                        }
                    },
                    #td{
                        style = MainStyle,
                        body = MainBody
                    }
                ]
            }
        end, [
            {<<"Contact">>, contact()},
            {<<"Privacy policy">>, privacy_policy()},
            {<<"License">>, license()}
        ])
    }.


contact() ->
    #link{
        style = <<"font-size: large;">>,
        body = <<?CONTACT_EMAIL>>, url = <<"mailto:",
            ?CONTACT_EMAIL>>
    }.


privacy_policy() ->
    #link{
        style = <<"font-size: large;">>,
        body = <<"Learn about privacy policy">>,
        url = ?PAGE_PRIVACY_POLICY
    }.



license() ->
    #span{
        style = <<"white-space: pre; font-size: large; line-height: initial">>,
        body = <<"MIT License
===========

    Copyright (C) 2016: ACK CYFRONET AGH

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the \"Software\"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.">>
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

event(init) ->
    ok;

event(terminate) ->
    ok.