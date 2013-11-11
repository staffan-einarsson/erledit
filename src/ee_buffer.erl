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
		to_string/1
		]
	).

-include_lib("eunit/include/eunit.hrl").
-include("ee_global.hrl").
-include("ee_buffer.hrl").
-include("ee_buffer_coords.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Creates a new empty ee_buffer object.
%% @end
%%--------------------------------------------------------------------
-spec new() -> ee_buffer().
new()
	->
		#ee_buffer{lines = [#ee_buffer_line{}]}.

new_test_()
	->
		[
		?_assertEqual(#ee_buffer{lines = [#ee_buffer_line{}]}, new())
		].

%%--------------------------------------------------------------------
%% @doc Creates an ee_buffer object from a string.
%% @end
%%--------------------------------------------------------------------
-spec create_from_string(Text :: string()) -> ee_buffer().
create_from_string(
		String
	)
	->
		#ee_buffer{lines = split_buffer(String)}.

%%--------------------------------------------------------------------

split_buffer(
		String
	)
	->
		split_buffer_loop(String, 1, [], []).

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------

create_from_string_test_()
	->
		[
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "", eol = none}
				]
			},
			create_from_string("")),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Hello", eol = none}
				]
			},
			create_from_string("Hello")),
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "Hello", eol = eol_lf},
				#ee_buffer_line{line_no = 2, contents = "World", eol = none}
			]},
			create_from_string("Hello\nWorld"))
		].

%%--------------------------------------------------------------------
%% @doc Inserts a string of text into an existing ee_buffer object.
%% @end
%%--------------------------------------------------------------------
-spec insert_text(Buffer :: ee_buffer(), Text :: string(), Coords :: ee_buffer_coords()) -> ee_buffer().
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

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @doc Inserts a line ending into an existing ee_buffer object.
%% @end
%%--------------------------------------------------------------------
-spec insert_eol(Buffer :: ee_buffer(), Coords :: ee_buffer_coords()) -> ee_buffer().
insert_eol(
		#ee_buffer{lines = Lines},
		#ee_buffer_coords{line_no = InsertLineNo}
	)
		when InsertLineNo > 1, InsertLineNo > length(Lines)
	->
		erlang:error(bad_buffer_coords);
insert_eol(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = InsertCoords
	)
	->
		Buffer#ee_buffer{lines = ?dbg_print(insert_eol_(Lines, InsertCoords, []))}.

%%--------------------------------------------------------------------

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
		erlang:error(bad_buffer_coords);
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

%%--------------------------------------------------------------------

insert_eol_test_()
	->
		[
		?_assertEqual(
			#ee_buffer{lines = [
				#ee_buffer_line{line_no = 1, contents = "", eol = eol_lf}
				]},
			insert_eol(
				#ee_buffer{lines = []},
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

%%--------------------------------------------------------------------
%% @doc Removes one character to the left of a given coordinate in an ee_buffer object.
%% @end
%%--------------------------------------------------------------------
-spec remove_left(Buffer :: ee_buffer(), Coords :: ee_buffer_coords()) -> ee_buffer().
remove_left(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = RemoveCoords
	)
	->
		Buffer#ee_buffer{lines = remove_left_(Lines, RemoveCoords, [])}.

%%--------------------------------------------------------------------

remove_left_(
		[],
		_,
		_
	)
	->
		erlang:error(bad_buffer_coords);
remove_left_(
		_,
		#ee_buffer_coords{line_no = 1, line_offset = 1},
		_
	)
	->
		erlang:error(bad_buffer_coords);
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
		erlang:error(bad_buffer_coords);
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

%%--------------------------------------------------------------------

remove_left_test_()
	->
		[
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 2)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_left(
				#ee_buffer{lines = []},
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

%%--------------------------------------------------------------------
%% @doc Removes one character to the right of a given coordinate in an ee_buffer object.
%% @end
%%--------------------------------------------------------------------
-spec remove_right(Buffer :: ee_buffer(), Coords :: ee_buffer_coords()) -> ee_buffer().
remove_right(
		#ee_buffer{lines = []},
		_
	)
	->
		erlang:error(bad_buffer_coords);
remove_right(
		#ee_buffer{lines = Lines} = Buffer,
		#ee_buffer_coords{} = RemoveCoords
	)
	->
		Buffer#ee_buffer{lines = remove_right_(Lines, RemoveCoords, [])}.

%%--------------------------------------------------------------------

%% An eol is being removed.
remove_right_(
		[#ee_buffer_line{line_no = RemoveLineNo, contents = Contents}|_],
		#ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset},
		_
	)
		when RemoveOffset > length(Contents) + 1
	->
		erlang:error(bad_buffer_coords);
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
	
%%--------------------------------------------------------------------

remove_right_test_()
	->
		[
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 1)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = []},
				ee_buffer_coords:new(1, 2)
				)
			),
		?_assertError(
			bad_buffer_coords,
			remove_right(
				#ee_buffer{lines = []},
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

%%--------------------------------------------------------------------
%% @doc Gets the line from an ee_buffer object given by a number.
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------

get_line_number(
		#ee_buffer_line{line_no = LineNo}
	)
	->
		LineNo.

get_line_number_test_()
	->
		[
		?_assertEqual(
			5,
			get_line_number(#ee_buffer_line{line_no = 5})
			)
		].

%%--------------------------------------------------------------------

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
		?_assertEqual(
			"Hello",
			get_line_contents(#ee_buffer_line{contents = "Hello"})
			)
		].

%%--------------------------------------------------------------------

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
		?_assertEqual(
			8,
			get_line_length(
				#ee_buffer{lines = [
					#ee_buffer_line{line_no = 1, contents = "Text", eol = eol_lf},
					#ee_buffer_line{line_no = 2, contents = "TextText", eol = eol_lf},
					#ee_buffer_line{line_no = 3, contents = "TextTextText", eol = none}
					]},
				2)
			),
		?_assertEqual(
			8,
			get_line_length(
				#ee_buffer_line{line_no = 2, contents = "TextText", eol = eol_lf}
				)
			)
		].

%%--------------------------------------------------------------------

get_num_lines(
		#ee_buffer{lines = Lines}
	)
	->
		length(Lines).

get_num_lines_test_()
	->
		[
		].

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------

to_string(
		#ee_buffer{lines = Lines}
	)
	->
		lines_to_string(Lines, []).

to_string_test_()
	->
		[
		].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------

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
