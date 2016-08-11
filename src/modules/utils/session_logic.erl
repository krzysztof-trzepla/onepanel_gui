%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements session_logic_behaviour and exports an
%% API for persisting GUI sessions.
%% @end
%% ===================================================================
-module(session_logic).
-behaviour(session_logic_behaviour).

-include("modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% session_logic_behaviour API
-export([init/0, cleanup/0]).
-export([save_session/3, lookup_session/1, delete_session/1, clear_expired_sessions/0]).
-export([get_cookie_ttl/0]).

%% ETS name for cookies
-define(SESSION_ETS, cookies).

%% ====================================================================
%% API functions
%% ====================================================================

init() ->
    % Ets table needed for session storing.
    ets:new(?SESSION_ETS, [named_table, public, bag, {read_concurrency, true}]),
    ok.


cleanup() ->
    ets:delete(?SESSION_ETS),
    ok.


save_session(SessionID, Props, TillArg) ->
    Till = case TillArg of
               undefined ->
                   case ets:lookup(?SESSION_ETS, SessionID) of
                       [{SessionID, _, CurrentTill}] ->
                           CurrentTill;
                       _ ->
                           throw("Session expiration not specified.")
                   end;
               _ ->
                   TillArg
           end,

    delete_session(SessionID),
    ets:insert(?SESSION_ETS, {SessionID, Props, Till}),
    ok.


lookup_session(SessionID) ->
    case ets:lookup(?SESSION_ETS, SessionID) of
        [{SessionID, Props, Till}] ->
            % Check if the session isn't outdated
            Now = erlang:monotonic_time(seconds),
            case Till > Now of
                true ->
                    Props;
                false ->
                    delete_session(SessionID),
                    undefined
            end;
        _ ->
            undefined
    end.


delete_session(SessionID) ->
    case SessionID of
        undefined ->
            ok;
        _ ->
            ets:delete(?SESSION_ETS, SessionID),
            ok
    end.


clear_expired_sessions() ->
    Now = erlang:monotonic_time(seconds),
    ExpiredSessions = ets:select(?SESSION_ETS, [{{'$1', '$2', '$3'}, [{'<', '$3', Now}], ['$_']}]),
    lists:foreach(
        fun({SessionID, _, _}) ->
            delete_session(SessionID)
        end, ExpiredSessions),
    length(ExpiredSessions).


get_cookie_ttl() ->
    case application:get_env(?APP_NAME, session_cookie_ttl) of
        {ok, Val} when is_integer(Val) ->
            Val;
        _ ->
            throw("No cookie TTL specified in env.")
    end.