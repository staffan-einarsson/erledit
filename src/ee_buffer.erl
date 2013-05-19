%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_buffer).

%% API
-export([new/0,	create_from_string/1, insert_text/4, get_line/2,
	get_line_number/1, get_line_contents/1, get_line_length/2,
	get_line_length/1, get_num_lines/1, foreach_line/2,
	insert_eol/3,
	remove_left/3,
	remove_right/3
	]).

-include("ee_global.hrl").
-include("ee_buffer.hrl").

%% ===================================================================
%% API
%% ===================================================================

new() ->
	#ee_buffer{}.

create_from_string(String) ->
	#ee_buffer{lines = split_buffer(String)}.

insert_text(#ee_buffer{lines = Lines} = Buffer, Text, LineNo, ColNo) ->
	Buffer#ee_buffer{lines = insert_text_(Lines, Text, LineNo, ColNo)}.

insert_eol(#ee_buffer{lines = Lines} = Buffer, LineNo, ColNo) ->
	Buffer#ee_buffer{lines = ?dbg_print(insert_eol_(Lines, LineNo, ColNo))}.

remove_left(#ee_buffer{lines = Lines} = Buffer, LineNo, ColNo) ->
	Buffer#ee_buffer{lines = remove_left_(Lines, LineNo, ColNo)}.
	
remove_right(#ee_buffer{lines = Lines} = Buffer, LineNo, ColNo) ->
	#ee_buffer_line{contents = LastLineContents} = lists:last(Lines),
	Buffer#ee_buffer{lines = remove_right_(Lines, LineNo, ColNo, length(LastLineContents))}.

get_line(#ee_buffer{lines = Lines}, LineNo) ->
	lists:nth(LineNo, Lines).

get_line_number(#ee_buffer_line{line_no = LineNo}) ->
	LineNo.

get_line_contents(#ee_buffer_line{contents = Contents}) ->
	Contents.

get_line_length(Buffer, LineNo) ->
	get_line_length(get_line(Buffer, LineNo)).

get_line_length(#ee_buffer_line{contents = Contents}) ->
	length(Contents).

get_num_lines(#ee_buffer{lines = Lines}) ->
	length(Lines).

foreach_line(#ee_buffer{lines = Lines}, Fun) ->
	lists:foreach(Fun, Lines).

%% ===================================================================
%% Internal functions
%% ===================================================================

split_buffer(String) ->
	split_buffer_loop(String, 0, [], []).

split_buffer_loop([], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	lists:reverse([#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = none}|PrevLinesRev]);
split_buffer_loop([?ASCII_LF|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_lf}|PrevLinesRev]);
split_buffer_loop([?ASCII_CR, ?ASCII_LF|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_crlf}|PrevLinesRev]);
split_buffer_loop([Char|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo, [Char|CurrentLineBufferRev], PrevLinesRev).

insert_text_([#ee_buffer_line{line_no = LineNo, contents = Contents} = Line|T], Text, LineNo, ColNo) ->
	{A, B} = lists:split(ColNo, Contents),
	[Line#ee_buffer_line{contents = A ++ (Text ++ B)}|T];
insert_text_([Line|T], Text, LineNo, ColNo) ->
	%% TODO: Tail recursion.
	[Line|insert_text_(T, Text, LineNo, ColNo)].

insert_eol_([], _, _) ->
	%% TODO: This makes it impossible to insert an eol in an empty buffer.
	[];
insert_eol_([#ee_buffer_line{line_no = LineNo} = Line|T], CaretLineNo, CaretColNo) when LineNo > CaretLineNo ->
	[Line#ee_buffer_line{line_no = LineNo + 1}|insert_eol_(T, CaretLineNo, CaretColNo)];
insert_eol_([#ee_buffer_line{line_no = CaretLineNo, contents = Contents, eol = Eol} = Line|T], CaretLineNo, CaretColNo) ->
	{A, B} = lists:split(CaretColNo, Contents),
	[Line#ee_buffer_line{contents = A, eol = eol_lf}, Line#ee_buffer_line{line_no = CaretLineNo + 1, contents = B, eol = Eol}|insert_eol_(T, CaretLineNo, CaretColNo)];
insert_eol_([#ee_buffer_line{} = Line|T], CaretLineNo, CaretColNo) ->
	[Line|insert_eol_(T, CaretLineNo, CaretColNo)].
	
remove_left_([], _, _) ->
	[];
%% We are at the beginning. No removal should be done.
remove_left_(Lines, 0, 0) ->
	Lines;
%% An eol is being removed.
%% TODO: Assumes now that lines appear in order, which might not be the case at all.
remove_left_([#ee_buffer_line{contents = ContentsFirst}, #ee_buffer_line{line_no = CaretLineNo, contents = ContentsSecond, eol = Eol}|T], CaretLineNo, 0) ->
	[#ee_buffer_line{line_no = CaretLineNo - 1, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|add_to_line_no(T, -1)];
%% A char is being removed.
remove_left_([#ee_buffer_line{line_no = CaretLineNo, contents = Contents} = Line|T], CaretLineNo, CaretColNo) ->
	{A, [_|B]} = lists:split(CaretColNo - 1, Contents),
	NewContents = A ++ B,
	[Line#ee_buffer_line{contents = NewContents}|T];
remove_left_([#ee_buffer_line{} = Line|T], CaretLineNo, CaretColNo) ->
	[Line|remove_left_(T, CaretLineNo, CaretColNo)].

%% We are at the end. No removal should be done.
remove_right_(Lines, CaretLineNo, CaretColNo, LastLineLength) when CaretLineNo == length(Lines) - 1, CaretColNo == LastLineLength ->
	Lines;
%% An eol is being removed.
remove_right_([#ee_buffer_line{line_no = CaretLineNo, contents = ContentsFirst}, #ee_buffer_line{contents = ContentsSecond, eol = Eol}|T], CaretLineNo, CaretColNo, _) when CaretColNo == length(ContentsFirst) ->
	[#ee_buffer_line{line_no = CaretLineNo, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|add_to_line_no(T, -1)];
%% A char is being removed.
remove_right_([#ee_buffer_line{line_no = CaretLineNo, contents = Contents} = Line|T], CaretLineNo, CaretColNo, _) ->
	{A, [_|B]} = lists:split(CaretColNo, Contents),
	NewContents = A ++ B,
	[Line#ee_buffer_line{contents = NewContents}|T];
remove_right_([#ee_buffer_line{} = Line|T], CaretLineNo, CaretColNo, LastLineLength) ->
	[Line|remove_right_(T, CaretLineNo, CaretColNo, LastLineLength)].
	
add_to_line_no([], _) ->
	[];
add_to_line_no([#ee_buffer_line{line_no = LineNo} = Line|T], Number) ->
	[Line#ee_buffer_line{line_no = LineNo + Number}|add_to_line_no(T, Number)].
