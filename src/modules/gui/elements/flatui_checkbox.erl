%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains definition of custom checkbox element.
%%% IMPORTANT: for the checkbox to work properly, JS function ".checkbox()"
%%% must be called on it. This is done automatically on every checkbox after page loads,
%%% but checkboxes added dynamically must be initialized with the function.
%%% This can be done easily using the init_checkbox function.
%%
%%% This file is taken from n2o and slightly modified.
%%% @end
%%%-------------------------------------------------------------------
-module(flatui_checkbox).

-include("modules/common.hrl").

%% API
-export([reflect/0, render_element/1, init_checkbox/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Used to list all record fields.
%% @end
%%--------------------------------------------------------------------
-spec reflect() -> [atom()].
reflect() ->
    record_info(fields, flatui_checkbox).

%%--------------------------------------------------------------------
%% @doc Produces HTML in binary.
%% @end
%%--------------------------------------------------------------------
-spec render_element(Record :: #flatui_checkbox{}) -> list().
render_element(Record) ->
    Id = case Record#flatui_checkbox.id of
             undefined -> wf:temp_id();
             I when is_binary(I) -> binary_to_list(I);
             I -> I
         end,
    case Record#flatui_checkbox.postback of
        undefined -> ignore;
        Postback ->
            Data = "[" ++ string:join([begin
                                           {Key, SourceId} = if
                                                                 is_atom(Src) ->
                                                                     S = atom_to_list(Src),
                                                                     {"atom('" ++ S ++ "')", S};
                                                                 true ->
                                                                     {"utf8.toByteArray('" ++ Src ++ "')", Src}
                                                             end,
                                           "tuple(" ++ Key ++ ", querySource('" ++ SourceId ++ "'))" end || Src <- Record#flatui_checkbox.source]
            ++ ["tuple(tuple(utf8.toByteArray('" ++ Id ++ "'), bin('detail')), event.detail)"], ",") ++ "]",
            Event = wf_event:new(Postback, Id, Record#flatui_checkbox.delegate, event, Data),
            wf:wire(wf:f("$('#~s').change(function (event){", [Id]) ++ Event ++ "});")
    end,
    Label = [wf_tags:emit_tag(<<"input">>, [], [
        % global
        {<<"accesskey">>, Record#flatui_checkbox.accesskey},
        {<<"class">>, Record#flatui_checkbox.class},
        {<<"contenteditable">>, case Record#flatui_checkbox.contenteditable of true ->
            "true"; false -> "false"; _ ->
            undefined end},
        {<<"contextmenu">>, Record#flatui_checkbox.contextmenu},
        {<<"dir">>, case Record#flatui_checkbox.dir of "ltr" -> "ltr"; "rtl" ->
            "rtl"; "auto" -> "auto"; _ ->
            undefined end},
        {<<"draggable">>, case Record#flatui_checkbox.draggable of true ->
            "true"; false -> "false"; _ ->
            undefined end},
        {<<"dropzone">>, Record#flatui_checkbox.dropzone},
        {<<"hidden">>, case Record#flatui_checkbox.hidden of "hidden" ->
            "hidden"; _ -> undefined end},
        {<<"id">>, Id},
        {<<"lang">>, Record#flatui_checkbox.lang},
        {<<"spellcheck">>, case Record#flatui_checkbox.spellcheck of true ->
            "true"; false -> "false"; _ ->
            undefined end},
        {<<"style">>, Record#flatui_checkbox.style},
        {<<"tabindex">>, Record#flatui_checkbox.tabindex},
        {<<"title">>, Record#flatui_checkbox.title},
        {<<"translate">>, case Record#flatui_checkbox.contenteditable of "yes" ->
            "yes"; "no" -> "no"; _ ->
            undefined end},
        % spec
        {<<"autofocus">>, Record#flatui_checkbox.autofocus},
        {<<"checked">>, if Record#flatui_checkbox.checked == true ->
            <<"checked">>; true -> undefined end},
        {<<"data-toggle">>, <<"checkbox">>},
        {<<"disabled">>, if Record#flatui_checkbox.disabled == true ->
            "disabled"; true -> undefined end},
        {<<"form">>, Record#flatui_checkbox.form},
        {<<"name">>, Record#flatui_checkbox.name},
        {<<"required">>, if Record#flatui_checkbox.required == true ->
            "required"; true -> undefined end},
        {<<"type">>, <<"checkbox">>},
        {<<"value">>, Record#flatui_checkbox.value} | Record#flatui_checkbox.data_fields
    ]),
        case Record#flatui_checkbox.body of undefined -> []; B -> B end],
    wf_tags:emit_tag(<<"label">>, wf:render(Label), [
        {<<"id">>, Record#flatui_checkbox.label_id},
        {<<"class">>, Record#flatui_checkbox.label_class},
        {<<"style">>, Record#flatui_checkbox.label_style},
        {<<"title">>, Record#flatui_checkbox.label_title},
        {<<"for">>, Id}]).

%%--------------------------------------------------------------------
%% @doc Initializes a checkbox with given id.
%% For the checkbox to work properly, it must be initialized first.
%% This is done automatically on every checkbox after page loads,
%% but checkboxes added dynamically must be initialized with the function.
%% @end
%%--------------------------------------------------------------------
-spec init_checkbox(ID :: binary()) -> ok.
init_checkbox(ID) ->
    gui_jq:wire(<<"$('#", ID/binary, "').checkbox();">>).
