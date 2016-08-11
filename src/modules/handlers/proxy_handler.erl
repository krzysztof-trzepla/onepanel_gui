%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Copy of n2o_cowboy.erl from n2o.
%% This is a cowboy handler module for handling HTTP request with n2o engine.
%% Compared to original, this module has slight changes in the function
%% cookie/2 - now its implementation uses custom session handler.
%% @end
%% ===================================================================
-module(proxy_handler).
-behaviour(cowboy_http_handler).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% Cowboy API
-export([init/3, handle/2, terminate/3]).

%% ====================================================================
%% cowboy_http_handler API
%% ====================================================================

init(_Transport, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    try
        {ok, Url} = application:get_env(?APP_NAME, onepanel_server_url),
        {Endpoint, Req2} = cowboy_req:path(Req),
        {Method, Req3} = cowboy_req:method(Req2),
        {Headers, Req4} = cowboy_req:headers(Req3),
        {ok, Body, Req5} = cowboy_req:body(Req4),
        {QueryStrings, Req6} = cowboy_req:qs(Req5),
        {ok, Status, RespHeaders, RespBody} = http_client:request(
            method(Method),
            <<Url/binary, Endpoint/binary, "?", QueryStrings/binary>>,
            Headers, Body, [insecure]
        ),
        {ok, Req7} = cowboy_req:reply(Status, RespHeaders, RespBody, Req6),
        {ok, Req7, State}
    catch T:M ->
        ?info_stacktrace("Error while proxing REST request - ~p:~p",
            [T, M]),
        {ok, ReqX} = cowboy_req:reply(500, [], <<"">>, Req),
        {ok, ReqX, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

method(<<"POST">>) -> post;
method(<<"PATCH">>) -> patch;
method(<<"GET">>) -> get;
method(<<"DELETE">>) -> delete;
method(<<"PUT">>) -> put.