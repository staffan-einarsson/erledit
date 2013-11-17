%% --------------------------------------------------------------------
%% @author Staffan <staffan.einarsson@gmail.com>
%% @copyright 2013 Staffan Einarsson
%% @doc
%% @end
%% --------------------------------------------------------------------
%% Copyright 2013 Staffan Einarsson
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% --------------------------------------------------------------------

-module(ee_doc_set).

%% API
-export([
	new/0,
	add_focus_document_view/2,
	remove_document_view/2,
	update_document_view_buffer/3,
	move_caret_left_in_focus_doc/1,
	move_caret_right_in_focus_doc/1,
	move_caret_up_in_focus_doc/1,
	move_caret_down_in_focus_doc/1,
	move_caret_to_line_start_in_focus_doc/1,
	move_caret_to_line_end_in_focus_doc/1,
	move_caret_up_one_page_in_focus_doc/1,
	move_caret_down_one_page_in_focus_doc/1,
	cycle_focus_doc/1
	]).

-include("ee_global.hrl").
-include("ee_doc_set.hrl").

%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

new() ->
	#ee_doc_set{}.

add_focus_document_view(#ee_doc_set{focus_doc = FocusDoc0, background_docs = BgDocs0} = DocSet, DocPid) ->
	FocusDoc1 = #ee_doc_view{pid = DocPid},
	BgDocs1 = case FocusDoc0 of
		undefined -> BgDocs0;
		_ -> [FocusDoc0|BgDocs0]
	end,
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc1, background_docs = BgDocs1}}.

remove_document_view(#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid}, background_docs = BgDocs0} = DocSet, DocPid) ->
	case BgDocs0 of
		[BgDoc|BgDocsT] ->
			FocusDoc1 = BgDoc,
			BgDocs1 = BgDocsT;
		[] ->
			FocusDoc1 = undefined,
			BgDocs1 = []
	end,
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc1, background_docs = BgDocs1}};
remove_document_view(#ee_doc_set{focus_doc = FocusDoc, background_docs = BgDocs0} = DocSet, DocPid) ->
	BgDocs1 = lists:filter(fun (Doc) -> case Doc of #ee_doc_view{pid = DocPid} -> false; _Other -> true end end, BgDocs0),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc, background_docs = BgDocs1}}.

update_document_view_buffer(#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid} = FocusDoc0, background_docs = BgDocs0} = DocSet, DocPid, Buffer) ->
	FocusDoc1 = FocusDoc0#ee_doc_view{buffer = Buffer},
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc1, background_docs = BgDocs0}};
update_document_view_buffer(#ee_doc_set{focus_doc = FocusDoc0, background_docs = BgDocs0} = DocSet, DocPid, Buffer) ->
	BgDocs1 = lists:map(fun (Doc) -> case Doc of #ee_doc_view{pid = DocPid} -> Doc#ee_doc_view{buffer = Buffer}; Other -> Other end end, BgDocs0),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc0, background_docs = BgDocs1}}.

move_caret_left_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_left(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_right_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_right(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_up_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_up(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_down_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_down(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_to_line_start_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_to_beginning_of_line(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_to_line_end_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_to_end_of_line(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_up_one_page_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_up_one_page(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

move_caret_down_one_page_in_focus_doc(#ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret0} = FocusDoc} = DocSet) ->
	Caret1 = ee_caret:move_down_one_page(Caret0, Buffer),
	{ok, DocSet#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}}}.

cycle_focus_doc(#ee_doc_set{background_docs = []} = DocSet) ->
	DocSet;
cycle_focus_doc(#ee_doc_set{focus_doc = FocusDoc0, background_docs = BgDocs0} = DocSet) ->
	[FocusDoc1|RevBgDocs1] = lists:reverse(BgDocs0),
	BgDocs2 = lists:reverse(RevBgDocs1),
	BgDocs3 = [FocusDoc0|BgDocs2],
	DocSet#ee_doc_set{focus_doc = FocusDoc1, background_docs = BgDocs3}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
