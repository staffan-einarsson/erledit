%%% -------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc
%%% @end
%%% -------------------------------------------------------------------
%%% Copyright 2013 Staffan Einarsson
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% -------------------------------------------------------------------

-module(ee_buffer).

%% API
-export(
		[
		new/0,
		new/1,
		create_from_string/1,
		insert_text/3,
		insert_eol/2,
		remove_left/2,
		remove_right/2,
		get_line/2,
		get_line_length/2,
		get_num_lines/1,
		foreach_line/2,
		to_string/1
		]
	).

-include_lib("eunit/include/eunit.hrl").
-include("ee_global.hrl").
-include("ee_buffer_coords.hrl").
-include("ee_buffer_line.hrl").
-include("ee_buffer.hrl").

%%% -------------------------------------------------------------------
%%% API
%%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% @doc Creates a new empty ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec new() -> ee_buffer().
new()
	->
		#ee_buffer{lines = [ee_buffer_line:new()]}.

%% --------------------------------------------------------------------
%% @doc Creates a new empty ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec new(Lines :: [ee_buffer_line()]) -> ee_buffer().
new(
		Lines
	)
	->
		#ee_buffer{lines = Lines}.

%% --------------------------------------------------------------------

new_test_()
	->
		Lines = [
			ee_buffer_line:new(1, "All", eol_lf),
			ee_buffer_line:new(1, "your", eol_lf),
			ee_buffer_line:new(1, "base", eol_lf)
			],
		[
		?_assertEqual(#ee_buffer{lines = [ee_buffer_line:new()]}, new()),
		?_assertEqual(#ee_buffer{lines = Lines}, new(Lines))
		].

%% --------------------------------------------------------------------
%% @doc Creates an ee_buffer object from a string.
%% @end
%% --------------------------------------------------------------------
-spec create_from_string(Text :: string()) -> ee_buffer().
create_from_string(
		String
	)
	->
		{CurrentLineNo, CurrentLineBufferRev, PrevLinesRev, _} = lists:foldl(fun process_char/2, {1, [], [], false}, String),
		FinalLines = case CurrentLineBufferRev of
			% [] -> 
				% lists:reverse(PrevLinesRev);
			_ ->
				lists:reverse([#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = none} | PrevLinesRev])
		end,
		#ee_buffer{lines = FinalLines}.

%% --------------------------------------------------------------------

process_char(
		?ASCII_LF,
		{CurrentLineNo, CurrentLineBufferRev, PrevLinesRev, false}
	)
	->
		{CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_lf} | PrevLinesRev], false};
process_char(
		?ASCII_LF,
		{CurrentLineNo, CurrentLineBufferRev, PrevLinesRev, true}
	)
	->
		{CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_crlf} | PrevLinesRev], false};
process_char(
		?ASCII_CR,
		{CurrentLineNo, CurrentLineBufferRev, PrevLinesRev, false}
	)
	->
		{CurrentLineNo, CurrentLineBufferRev, PrevLinesRev, true};
process_char(
		Char,
		{CurrentLineNo, CurrentLineBufferRev, PrevLinesRev, _}
	)
	->
		{CurrentLineNo, [Char|CurrentLineBufferRev], PrevLinesRev, false}.

%% --------------------------------------------------------------------

create_from_string_test_()
	->
		[
		?_assertEqual(
			#ee_buffer{lines = [ee_buffer_line:new(1, "", none)]},
			create_from_string("")
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Hello", eol = none}
				]
			},
			create_from_string("Hello")
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Hello", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "", eol = none}
				]
			},
			create_from_string("Hello\n")
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Hello", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "World", eol = none}
			]},
			create_from_string("Hello\nWorld")
			)
		].

%% --------------------------------------------------------------------
%% @doc Inserts a string of text into an existing ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec insert_text(Buffer :: ee_buffer(), Text :: string(), Coords :: ee_buffer_coords()) -> ee_buffer().
insert_text(
		#ee_buffer{lines = Lines},
		_,
		#ee_buffer_coords{line_no = InsertLineNo}
	)
		when InsertLineNo > length(Lines)
	->
		error_bad_buffer_coords();
insert_text(
		#ee_buffer{lines = Lines} = Buffer,
		Text,
		#ee_buffer_coords{} = InsertCoords
	)
	->
		{NewLinesRev, _, _} = lists:foldl(fun insert_text_on_line/2, {[], InsertCoords, Text}, Lines),
		Buffer#ee_buffer{lines = lists:reverse(NewLinesRev)}.

%% --------------------------------------------------------------------

insert_text_on_line(
		#ee_buffer_line{line_no = InsertLineNo, contents = Contents},
		{_, #ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset}, _}
	)
		when InsertOffset > length(Contents) + 1
	->
		error_bad_buffer_coords();
insert_text_on_line(
		#ee_buffer_line{line_no = InsertLineNo, contents = Contents} = Line,
		{PrevLinesRev, #ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset} = InsertCoords, Text}
	)
	->
		{A, B} = lists:split(InsertOffset - 1, Contents),
		{[Line#ee_buffer_line{contents = A ++ (Text ++ B)} | PrevLinesRev], InsertCoords, Text};
insert_text_on_line(
		Line,
		{PrevLinesRev, InsertCoords, Text}
	)
	->
		{[Line | PrevLinesRev], InsertCoords, Text}.

%% --------------------------------------------------------------------

insert_text_test_()
	->
		[
		?_assertEqual(
			#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "TextAfterText", eol = none}]},
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "AfterText", ee_buffer_coords:new(1, 5))
			),
		?_assertEqual(
			#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "TextBeforeText", eol = none}]},
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "TextBefore", ee_buffer_coords:new(1, 1))
			),
		?_assertEqual(
			#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Line1", eol = eol_lf}, #ee_buffer_line{line_no = 2, contents = "LiXXne2", eol = none}]},
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Line1", eol = eol_lf}, #ee_buffer_line{line_no = 2, contents = "Line2", eol = none}]}, "XX", ee_buffer_coords:new(2, 3))
			),
		?_assertError(bad_buffer_coords,
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "NewText", ee_buffer_coords:new(2, 1))
			),
		?_assertError(bad_buffer_coords,
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "NewText", ee_buffer_coords:new(1, 10))
			)
		].

%% --------------------------------------------------------------------
%% @doc Inserts a line ending into an existing ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec insert_eol(Buffer :: ee_buffer(), Coords :: ee_buffer_coords()) -> ee_buffer().
insert_eol(
		#ee_buffer{lines = []},
		_
	)
	->
		error_invalid_buffer();
insert_eol(
		#ee_buffer{lines = Lines},
		#ee_buffer_coords{line_no = InsertLineNo}
	)
		when InsertLineNo > 1, InsertLineNo > length(Lines)
	->
		error_bad_buffer_coords();
insert_eol(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = InsertCoords
	)
	->
		Buffer#ee_buffer{lines = insert_eol_(Lines, InsertCoords, [])}.

%% --------------------------------------------------------------------

insert_eol_(
		[],
		_,
		[]
	)
	->
		[#ee_buffer_line{line_no = 1, contents = "", eol = eol_lf}];
insert_eol_(
		[#ee_buffer_line{line_no = InsertLineNo, contents = Contents} = Line],
		#ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset},
		Res
	)
		when InsertOffset == length(Contents) + 1
	->
		lists:reverse([Line#ee_buffer_line{eol = eol_lf}|Res]);
insert_eol_(
		[#ee_buffer_line{line_no = InsertLineNo, contents = Contents}|_],
		#ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset},
		_
	)
		when InsertOffset > length(Contents) + 1
	->
		error_bad_buffer_coords();
insert_eol_(
		[#ee_buffer_line{line_no = InsertLineNo, contents = Contents, eol = Eol} = Line|T],
		#ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset},
		Res
	)
	->
		{A, B} = lists:split(InsertOffset - 1, Contents),
		add_to_line_no(T, 1, [Line#ee_buffer_line{line_no = InsertLineNo + 1, contents = B, eol = Eol}, Line#ee_buffer_line{contents = A, eol = eol_lf}|Res]);
insert_eol_(
		[Line|T],
		InsertCoords,
		Res
	)
	->
		insert_eol_(T, InsertCoords, [Line|Res]).

%% --------------------------------------------------------------------

add_to_line_no(
		[],
		_,
		Res
	)
	->
		lists:reverse(Res);
add_to_line_no(
		[#ee_buffer_line{line_no = LineNo} = Line|T],
		Number,
		Res
	)
	->
		add_to_line_no(T, Number, [Line#ee_buffer_line{line_no = LineNo + Number}|Res]).

%% --------------------------------------------------------------------

insert_eol_test_()
	->
		[
		?_assertError(
			invalid_buffer,
			insert_eol(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 1))
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "", eol = eol_lf}
				]},
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(1, 1))
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "Text", eol = none}
				]},
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = none}
					]},
				ee_buffer_coords:new(1, 1))
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Te", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "xt", eol = none}
				]},
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = none}
					]},
				ee_buffer_coords:new(1, 3))
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
				]},
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = none}
					]},
				ee_buffer_coords:new(1, 5))
			),
		?_assertError(
			bad_buffer_coords,
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = none}
					]},
				ee_buffer_coords:new(1, 6))
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "", eol = eol_lf},
				#ee_buffer_line{line_no = 3, contents = "Text2", eol = eol_lf},
				#ee_buffer_line{line_no = 4, contents = "Text3", eol = none}
				]},
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = none}
					]},
				ee_buffer_coords:new(2, 1))
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
				#ee_buffer_line{line_no = 3, contents = "", eol = eol_lf},
				#ee_buffer_line{line_no = 4, contents = "Text3", eol = none}
				]},
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = none}
					]},
				ee_buffer_coords:new(2, 6))
			),
		?_assertError(
			bad_buffer_coords,
			insert_eol(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = none}
					]},
				ee_buffer_coords:new(4, 1))
			)
		].

%% --------------------------------------------------------------------
%% @doc Removes one character to the left of a given coordinate in an ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec remove_left(Buffer :: ee_buffer(), Coords :: ee_buffer_coords()) -> ee_buffer().
remove_left(
		#ee_buffer{lines = []},
		_
	)
	->
		error_invalid_buffer();
remove_left(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = RemoveCoords
	)
	->
		Buffer#ee_buffer{lines = remove_left_(Lines, RemoveCoords, [])}.

%% --------------------------------------------------------------------

remove_left_(
		[],
		_,
		_
	)
	->
		error_bad_buffer_coords();
remove_left_(
		_,
		#ee_buffer_coords{line_no = 1, line_offset = 1},
		_
	)
	->
		error_bad_buffer_coords();
%% An eol is being removed.
%% TODO: Assumes now that lines appear in order, which might not be the case at all.
remove_left_(
		[#ee_buffer_line{contents = ContentsFirst}, #ee_buffer_line{line_no = RemoveLineNo, contents = ContentsSecond, eol = Eol}|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = 1},
		Res
	)
	->
		add_to_line_no(T, -1, [#ee_buffer_line{line_no = RemoveLineNo - 1, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|Res]);
%% A char is being removed.
remove_left_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents}|_],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		_
	)
		when RemoveOffset > length(Contents) + 1
	->
		error_bad_buffer_coords();
remove_left_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		Res
	)
	->
		{A, [_|B]} = lists:split(RemoveOffset - 2, Contents),
		NewContents = A ++ B,
		lists:reverse([Line#ee_buffer_line{contents = NewContents}|Res]) ++ T;
remove_left_(
		[Line|T],
		RemoveCoords,
		Res
	)
	->
		remove_left_(T, RemoveCoords, [Line|Res]).

%% --------------------------------------------------------------------

remove_left_test_()
	->
		[
		?_assertError(
			invalid_buffer,
			remove_left(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(1, 2)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(2, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "ext", eol = eol_lf}
				]},
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 2)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Tex", eol = eol_lf}
				]},
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 5)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 6)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1Text2", eol = eol_crlf},
				#ee_buffer_line{line_no = 2, contents = "Text3", eol = eol_lf}
				]},
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_crlf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
					]},
				ee_buffer_coords:new(2, 1)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "Txt2", eol = eol_lf},
				#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
				]},
			remove_left(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
					]},
				ee_buffer_coords:new(2, 3)
				)
			)
		].

%% --------------------------------------------------------------------
%% @doc Removes one character to the right of a given coordinate in an ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec remove_right(Buffer :: ee_buffer(), Coords :: ee_buffer_coords()) -> ee_buffer().
remove_right(
		#ee_buffer{lines = []},
		_
	)
	->
		error_invalid_buffer();
remove_right(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = RemoveCoords
	)
	->
		Buffer#ee_buffer{lines = remove_right_(Lines, RemoveCoords, [])}.

%% --------------------------------------------------------------------

remove_right_(
		[],
		_,
		_
	)
	->
		error_bad_buffer_coords();
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents}|_],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		_
	)
		when RemoveOffset > length(Contents) + 1
	->
		error_bad_buffer_coords();
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents, eol = none}|_],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		_
	)
		when RemoveOffset > length(Contents)
	->
		error_bad_buffer_coords();
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = ContentsFirst}, #ee_buffer_line{contents = ContentsSecond, eol = Eol}|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		Res
	)
		when RemoveOffset == length(ContentsFirst) + 1
	->
		add_to_line_no(T, -1, [#ee_buffer_line{line_no = RemoveLineNo, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|Res]);
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		Res
	)
		when RemoveOffset == length(Contents) + 1 
	->
		lists:reverse([Line#ee_buffer_line{eol = none}|Res]);
%% A char is being removed.		
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		Res
	)
	->
		{A, [_|B]} = lists:split(RemoveOffset - 1, Contents),
		NewContents = A ++ B,
		lists:reverse([Line#ee_buffer_line{contents = NewContents}|Res]) ++ T;
remove_right_(
		[Line|T],
		RemoveCoords,
		Res
	)
	->
		remove_right_(T, RemoveCoords, [Line|Res]).
	
%% --------------------------------------------------------------------

remove_right_test_()
	->
		[
		?_assertError(
			invalid_buffer,
			remove_right(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(1, 2)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				ee_buffer_coords:new(2, 1)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "ext", eol = eol_lf}
				]},
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Tet", eol = eol_lf}
				]},
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 3)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text", eol = none}
				]},
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 5)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf}
					]},
				ee_buffer_coords:new(1, 6)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "Text2Text3", eol = eol_lf}
				]},
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_crlf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
					]},
				ee_buffer_coords:new(2, 6)
				)
			),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "Tet2", eol = eol_lf},
				#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
				]},
			remove_right(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
					]},
				ee_buffer_coords:new(2, 3)
				)
			)
		].

%% --------------------------------------------------------------------
%% @doc Gets the line from an ee_buffer object given by a number.
%% @end
%% --------------------------------------------------------------------
-spec get_line(Buffer :: ee_buffer(), LineNo :: integer()) -> ee_buffer_line().
get_line(
		#ee_buffer{lines = Lines},
		LineNo
	)
		when LineNo >= 1, LineNo =< length(Lines)
	->
		lists:nth(LineNo, Lines);
get_line(
		_,
		_
	)
	->
		erlang:error(bad_line_number).

%% --------------------------------------------------------------------

get_line_test_()
	->
		[
		?_assertEqual(#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
			get_line(#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "Text2", eol = eol_lf},
				#ee_buffer_line{line_no = 3, contents = "Text3", eol = eol_lf}
				]}, 2)
			),
		?_assertError(bad_line_number,
			get_line(#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf}
				]}, 0)
			),
		?_assertError(bad_line_number,
			get_line(#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf}
				]}, -2)
			),
		?_assertError(bad_line_number,
			get_line(#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Text1", eol = eol_lf}
				]}, 2)
			)
		].

%% --------------------------------------------------------------------
%% @doc Gets the length of a line from an ee_buffer object given by a line number.
%% @end
%% --------------------------------------------------------------------
-spec get_line_length(Buffer :: ee_buffer(), LineNo :: integer()) -> integer().
get_line_length(
		#ee_buffer{lines = []},
		_
	)
	->
		error_invalid_buffer();
get_line_length(
		Buffer,
		LineNo
	)
	->
		ee_buffer_line:get_line_length(get_line(Buffer, LineNo)).

%% --------------------------------------------------------------------

get_line_length_test_()
	->
		[
		?_assertError(
			invalid_buffer,
			get_line_length(#ee_buffer{lines = []},	1)),
		?_assertEqual(
			0,
			get_line_length(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				1)),
		?_assertError(
			bad_line_number,
			get_line_length(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "", eol = none}
					]},
				2)),
		?_assertEqual(
			8,
			get_line_length(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "TextText", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "TextTextText", eol = none}
					]},
				2)
			)
		].

%% --------------------------------------------------------------------
%% @doc Gets the number of lines in an ee_buffer object.
%% @end
%% --------------------------------------------------------------------
-spec get_num_lines(Buffer :: ee_buffer()) -> integer().
get_num_lines(
		#ee_buffer{lines = []}
	)
	->
		error_invalid_buffer();
get_num_lines(
		#ee_buffer{lines = Lines}
	)
	->
		length(Lines).

%% --------------------------------------------------------------------

get_num_lines_test_()
	->
		[
		?_assertError(
			invalid_buffer,
			get_num_lines(
				#ee_buffer{lines = []}
				)
			),
		?_assertEqual(
			1,
			get_num_lines(
				#ee_buffer{lines = [
					ee_buffer_line:new(1, "", none)
					]}
				)
			),
		?_assertEqual(
			5,
			get_num_lines(
				#ee_buffer{lines = [
					ee_buffer_line:new(1, "Text1", eol_lf),
					ee_buffer_line:new(2, "Text2", eol_lf),
					ee_buffer_line:new(3, "Text3", eol_lf),
					ee_buffer_line:new(4, "Text4", eol_lf),
					ee_buffer_line:new(5, "Text5", eol_lf)
					]}
				)
			)
		].

%% --------------------------------------------------------------------
%% @doc Calls Fun(Line) for each line in Buffer.
%% @end
%% --------------------------------------------------------------------
-spec foreach_line(Buffer :: ee_buffer(), Fun :: fun((Line :: ee_buffer_line()) -> term())) -> ok.
foreach_line(
		#ee_buffer{lines = Lines},
		Fun
	)
	->
		lists:foreach(Fun, Lines).

%% --------------------------------------------------------------------

foreach_line_test_()
	->
		[
		].

%% --------------------------------------------------------------------
%% @doc Converts Buffer to a string.
%% @end
%% --------------------------------------------------------------------
-spec to_string(Buffer :: ee_buffer()) -> string().
to_string(
		#ee_buffer{lines = []}
	)
	->
		error_invalid_buffer();
to_string(
		#ee_buffer{lines = Lines}
	)
	->
		lines_to_string(Lines, []).

%% --------------------------------------------------------------------

to_string_test_()
	->
		[
		?_assertError(
			invalid_buffer,
			to_string(#ee_buffer{lines = []})
			),
		?_assertEqual(
			"",
			to_string(
				#ee_buffer{lines = [
					ee_buffer_line:new(1, "", none)
					]}
				)
			),
		?_assertEqual(
			"Bapp1\nBapp2\nBapp3\n",
			to_string(
				#ee_buffer{lines = [
					ee_buffer_line:new(1, "Bapp1", eol_lf),
					ee_buffer_line:new(2, "Bapp2", eol_lf),
					ee_buffer_line:new(3, "Bapp3", eol_lf)
					]}
				)
			)
		].

%% --------------------------------------------------------------------

lines_to_string(
		[],
		AccText
	)
	->
		AccText;
lines_to_string(
		[#ee_buffer_line{contents = Text, eol = Eol}|T],
		AccText
	)
	->
		% TODO: Optimize using deep lists to avoid copying
		lines_to_string(T, AccText ++ Text ++ encode_eol(Eol)).

%% --------------------------------------------------------------------

encode_eol(
		none
	)
	->
		[];
encode_eol(
		eol_lf
	)
	->
		[?ASCII_LF];
encode_eol(
		eol_crlf
	)
	->
		[?ASCII_CR, ?ASCII_LF].

%% --------------------------------------------------------------------

error_invalid_buffer()
	->
		erlang:error(invalid_buffer).

%% --------------------------------------------------------------------

error_bad_buffer_coords()
	->
		erlang:error(bad_buffer_coords).