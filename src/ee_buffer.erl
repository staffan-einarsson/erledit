%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_buffer).

%% API
-export(
		[
		new/0,
		create_from_string/1,
		insert_text/3,
		insert_eol/2,
		remove_left/2,
		remove_right/2,
		get_line/2,
		get_line_number/1,
		get_line_contents/1,
		get_line_length/2,
		get_line_length/1,
		get_num_lines/1,
		foreach_line/2,
		new_buffer_coords/2,
		to_string/1
		]
	).

-include_lib("eunit/include/eunit.hrl").
-include("ee_global.hrl").
-include("ee_buffer.hrl").

%% ===================================================================
%% API
%% ===================================================================

new()
	->
		#ee_buffer{lines = [#ee_buffer_line{}]}.

new_test_()
	->
		[
		?_assertEqual(#ee_buffer{lines = [#ee_buffer_line{}]}, new())
		].

create_from_string(
		String
	)
	->
		#ee_buffer{lines = split_buffer(String)}.

create_from_string_test_()
	->
		[
		?_assertEqual(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "", eol = none}]}, create_from_string("")),
		?_assertEqual(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Hello", eol = none}]}, create_from_string("Hello")),
		?_assertEqual(#ee_buffer{lines = [
			#ee_buffer_line{line_no = 1, contents = "Hello", eol = eol_lf},
			#ee_buffer_line{line_no = 2, contents = "World", eol = none}
			]},
			create_from_string("Hello\nWorld"))
		].

insert_text(
		#ee_buffer{lines = Lines},
		_,
		#ee_buffer_coords{line_no = InsertLineNo}
	)
		when InsertLineNo > length(Lines)
	->
		erlang:error(bad_buffer_coords);
insert_text(
		#ee_buffer{lines = Lines} = Buffer,
		Text,
		#ee_buffer_coords{} = InsertCoords
	)
	->
		Buffer#ee_buffer{lines = insert_text_(Lines, Text, InsertCoords)}.

insert_text_test_()
	->
		[
		?_assertEqual(
			#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "TextAfterText", eol = none}]},
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "AfterText", #ee_buffer_coords{line_no = 1, line_offset = 5})
			),
		?_assertEqual(
			#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "TextBeforeText", eol = none}]},
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "TextBefore", #ee_buffer_coords{line_no = 1, line_offset = 1})
			),
		?_assertEqual(
			#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Line1", eol = eol_lf}, #ee_buffer_line{line_no = 2, contents = "LiXXne2", eol = none}]},
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Line1", eol = eol_lf}, #ee_buffer_line{line_no = 2, contents = "Line2", eol = none}]}, "XX", #ee_buffer_coords{line_no = 2, line_offset = 3})
			),
		?_assertError(bad_buffer_coords,
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "NewText", #ee_buffer_coords{line_no = 2, line_offset = 1})
			),
		?_assertError(bad_buffer_coords,
			insert_text(#ee_buffer{lines = [#ee_buffer_line{line_no = 1, contents = "Text", eol = none}]}, "NewText", #ee_buffer_coords{line_no = 1, line_offset = 10})
			)
		].

insert_eol(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = InsertCoords
	)
	->
		Buffer#ee_buffer{lines = ?dbg_print(insert_eol_(Lines, InsertCoords))}.

insert_eol_test_()
	->
		[
		].

remove_left(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = RemoveCoords
	)
	->
		Buffer#ee_buffer{lines = remove_left_(Lines, RemoveCoords)}.

remove_left_test_()
	->
		[
		].

remove_right(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = RemoveCoords
	)
	->
		#ee_buffer_line{contents = LastLineContents} = lists:last(Lines),
		Buffer#ee_buffer{lines = remove_right_(Lines, RemoveCoords, length(LastLineContents))}.

remove_right_test_()
	->
		[
		].

get_line(
		#ee_buffer{lines = Lines},
		LineNo
	)
		when LineNo >= 1, LineNo =< length(Lines)
	->
		lists:nth(LineNo, Lines);
get_line(
		_Buffer,
		_LineNo
	)
	->
		invalid_line.

get_line_test_()
	->
		[
		].

get_line_number(
		#ee_buffer_line{line_no = LineNo}
	)
	->
		LineNo.

get_line_number_test_()
	->
		[
		].

get_line_contents(
		invalid_line
	)
	->
		no_contents;
get_line_contents(
		#ee_buffer_line{contents = Contents}
	)
	->
		Contents.

get_line_contents_test_()
	->
		[
		].

get_line_length(
		Buffer,
		LineNo
	)
	->
		get_line_length(get_line(Buffer, LineNo)).

get_line_length(
		#ee_buffer_line{contents = Contents}
	)
	->
		length(Contents);
get_line_length(
		invalid_line
	)
	->
		invalid_line.

get_line_length_test_()
	->
		[
		].

get_num_lines(
		#ee_buffer{lines = Lines}
	)
	->
		length(Lines).

get_num_lines_test_()
	->
		[
		].

foreach_line(
		#ee_buffer{lines = Lines},
		Fun
	)
	->
		lists:foreach(Fun, Lines).

foreach_line_test_()
	->
		[
		].

new_buffer_coords(
		LineNo,
		LineOffset
	)
	->
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}.

new_buffer_coords_test_()
	->
		[
		].

to_string(
		#ee_buffer{lines = Lines}
	)
	->
		lines_to_string(Lines, []).

to_string_test_()
	->
		[
		].

%% ===================================================================
%% Internal functions
%% ===================================================================

split_buffer(
		String
	)
	->
		split_buffer_loop(String, 1, [], []).

split_buffer_loop(
		[],
		CurrentLineNo,
		CurrentLineBufferRev,
		PrevLinesRev
	)
	->
		lists:reverse([#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = none}|PrevLinesRev]);
split_buffer_loop(
		[?ASCII_LF|T],
		CurrentLineNo,
		CurrentLineBufferRev,
		PrevLinesRev
	)
	->
		split_buffer_loop(T, CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_lf}|PrevLinesRev]);
split_buffer_loop(
		[?ASCII_CR, ?ASCII_LF|T],
		CurrentLineNo,
		CurrentLineBufferRev,
		PrevLinesRev
	)
	->
		split_buffer_loop(T, CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_crlf}|PrevLinesRev]);
split_buffer_loop(
		[Char|T],
		CurrentLineNo,
		CurrentLineBufferRev,
		PrevLinesRev
	)
	->
		split_buffer_loop(T, CurrentLineNo, [Char|CurrentLineBufferRev], PrevLinesRev).

insert_text_(
		[#ee_buffer_line{line_no = InsertLineNo, contents = Contents}|_],
		_,
		#ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset}
	)
		when InsertOffset > length(Contents) + 1
	->
		erlang:error(bad_buffer_coords);
insert_text_(
		[#ee_buffer_line{line_no = InsertLineNo, contents = Contents} = Line|T],
		Text,
		#ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset}
	)
	->
		{A, B} = lists:split(InsertOffset - 1, Contents),
		[Line#ee_buffer_line{contents = A ++ (Text ++ B)}|T];
insert_text_(
		[Line|T],
		Text,
		InsertCoords
	)
	->
		%% TODO: Tail recursion.
		[Line|insert_text_(T, Text, InsertCoords)].

insert_eol_(
		[],
		_
	)
	->
		%% TODO: This makes it impossible to insert an eol in an empty buffer.
		[];
insert_eol_(
		[#ee_buffer_line{line_no = LineNo} = Line|T],
		#ee_buffer_coords{line_no = InsertLineNo} = InsertCoords
	)
		when LineNo > InsertLineNo
	->
		[Line#ee_buffer_line{line_no = LineNo + 1}|insert_eol_(T, InsertCoords)];
insert_eol_(
		[#ee_buffer_line{line_no = InsertLineNo, contents = Contents, eol = Eol} = Line|T],
		#ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset} = InsertCoords
	)
	->
		{A, B} = lists:split(InsertOffset - 1, Contents),
		[Line#ee_buffer_line{contents = A, eol = eol_lf}, Line#ee_buffer_line{line_no = InsertLineNo + 1, contents = B, eol = Eol}|insert_eol_(T, InsertCoords)];
insert_eol_(
		[Line|T],
		InsertCoords
	)
	->
		[Line|insert_eol_(T, InsertCoords)].
	
remove_left_(
		[],
		_
	)
	->
		[];
%% We are at the beginning. No removal should be done.
remove_left_(
		Lines,
		#ee_buffer_coords{line_no = 1, line_offset = 1}
	)
	->
		Lines;
%% An eol is being removed.
%% TODO: Assumes now that lines appear in order, which might not be the case at all.
remove_left_(
		[#ee_buffer_line{contents = ContentsFirst}, #ee_buffer_line{line_no = RemoveLineNo, contents = ContentsSecond, eol = Eol}|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = 1}
	)
	->
		[#ee_buffer_line{line_no = RemoveLineNo - 1, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|add_to_line_no(T, -1)];
%% A char is being removed.
remove_left_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset}
	)
	->
		{A, [_|B]} = lists:split(RemoveOffset - 2, Contents),
		NewContents = A ++ B,
		[Line#ee_buffer_line{contents = NewContents}|T];
remove_left_(
		[Line|T],
		RemoveCoords
	)
	->
		[Line|remove_left_(T, RemoveCoords)].

%% We are at the end. No removal should be done.
remove_right_(
		Lines,
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		LastLineLength
	)
		when RemoveLineNo == length(Lines), RemoveOffset == LastLineLength + 1
	->
		Lines;
%% An eol is being removed.
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = ContentsFirst}, #ee_buffer_line{contents = ContentsSecond, eol = Eol}|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		_
	)
		when RemoveOffset == length(ContentsFirst) + 1
	->
		[#ee_buffer_line{line_no = RemoveLineNo, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|add_to_line_no(T, -1)];
%% A char is being removed.
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line|T],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		_
	)
	->
		{A, [_|B]} = lists:split(RemoveOffset - 1, Contents),
		NewContents = A ++ B,
		[Line#ee_buffer_line{contents = NewContents}|T];
remove_right_(
		[Line|T],
		RemoveCoords,
		LastLineLength
	)
	->
		[Line|remove_right_(T, RemoveCoords, LastLineLength)].
	
add_to_line_no(
		[],
		_
	)
	->
		[];
add_to_line_no(
		[#ee_buffer_line{line_no = LineNo} = Line|T],
		Number
	)
	->
		[Line#ee_buffer_line{line_no = LineNo + Number}|add_to_line_no(T, Number)].

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
		lines_to_string(T, AccText ++ Text ++ encode_eol(Eol)).

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
