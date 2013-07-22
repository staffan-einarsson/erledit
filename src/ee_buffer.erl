%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_buffer).

%% API
-export([new/0,
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

insert_text(#ee_buffer{lines = Lines} = Buffer, Text, #ee_buffer_coords{} = InsertCoords) ->
	Buffer#ee_buffer{lines = insert_text_(Lines, Text, InsertCoords)}.

insert_eol(#ee_buffer{lines = Lines} = Buffer, #ee_buffer_coords{} = InsertCoords) ->
	Buffer#ee_buffer{lines = ?dbg_print(insert_eol_(Lines, InsertCoords))}.

remove_left(#ee_buffer{lines = Lines} = Buffer, #ee_buffer_coords{} = RemoveCoords) ->
	Buffer#ee_buffer{lines = remove_left_(Lines, RemoveCoords)}.
	
remove_right(#ee_buffer{lines = Lines} = Buffer, #ee_buffer_coords{} = RemoveCoords) ->
	#ee_buffer_line{contents = LastLineContents} = lists:last(Lines),
	Buffer#ee_buffer{lines = remove_right_(Lines, RemoveCoords, length(LastLineContents))}.

get_line(#ee_buffer{lines = Lines}, LineNo) when LineNo >= 1, LineNo =< length(Lines) ->
	lists:nth(LineNo, Lines);
get_line(_Buffer, _LineNo) ->
	invalid_line.

get_line_number(#ee_buffer_line{line_no = LineNo}) ->
	LineNo.

get_line_contents(invalid_line) ->
	no_contents;
get_line_contents(#ee_buffer_line{contents = Contents}) ->
	Contents.

get_line_length(Buffer, LineNo) ->
	get_line_length(get_line(Buffer, LineNo)).

get_line_length(#ee_buffer_line{contents = Contents}) ->
	length(Contents);
get_line_length(invalid_line) ->
	invalid_line.

get_num_lines(#ee_buffer{lines = Lines}) ->
	length(Lines).

foreach_line(#ee_buffer{lines = Lines}, Fun) ->
	lists:foreach(Fun, Lines).

new_buffer_coords(LineNo, LineOffset) ->
	#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}.

to_string(#ee_buffer{lines = Lines}) ->
	lines_to_string(Lines, []).

%% ===================================================================
%% Internal functions
%% ===================================================================

split_buffer(String) ->
	split_buffer_loop(String, 1, [], []).

split_buffer_loop([], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	lists:reverse([#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = none}|PrevLinesRev]);
split_buffer_loop([?ASCII_LF|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_lf}|PrevLinesRev]);
split_buffer_loop([?ASCII_CR, ?ASCII_LF|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo + 1, [], [#ee_buffer_line{line_no = CurrentLineNo, contents = lists:reverse(CurrentLineBufferRev), eol = eol_crlf}|PrevLinesRev]);
split_buffer_loop([Char|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo, [Char|CurrentLineBufferRev], PrevLinesRev).

insert_text_([#ee_buffer_line{line_no = InsertLineNo, contents = Contents} = Line|T], Text, #ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset}) ->
	{A, B} = lists:split(InsertOffset - 1, Contents),
	[Line#ee_buffer_line{contents = A ++ (Text ++ B)}|T];
insert_text_([Line|T], Text, InsertCoords) ->
	%% TODO: Tail recursion.
	[Line|insert_text_(T, Text, InsertCoords)].

insert_eol_([], _) ->
	%% TODO: This makes it impossible to insert an eol in an empty buffer.
	[];
insert_eol_([#ee_buffer_line{line_no = LineNo} = Line|T], #ee_buffer_coords{line_no = InsertLineNo} = InsertCoords) when LineNo > InsertLineNo ->
	[Line#ee_buffer_line{line_no = LineNo + 1}|insert_eol_(T, InsertCoords)];
insert_eol_([#ee_buffer_line{line_no = InsertLineNo, contents = Contents, eol = Eol} = Line|T], #ee_buffer_coords{line_no = InsertLineNo, line_offset = InsertOffset} = InsertCoords) ->
	{A, B} = lists:split(InsertOffset - 1, Contents),
	[Line#ee_buffer_line{contents = A, eol = eol_lf}, Line#ee_buffer_line{line_no = InsertLineNo + 1, contents = B, eol = Eol}|insert_eol_(T, InsertCoords)];
insert_eol_([Line|T], InsertCoords) ->
	[Line|insert_eol_(T, InsertCoords)].
	
remove_left_([], _) ->
	[];
%% We are at the beginning. No removal should be done.
remove_left_(Lines, #ee_buffer_coords{line_no = 1, line_offset = 1}) ->
	Lines;
%% An eol is being removed.
%% TODO: Assumes now that lines appear in order, which might not be the case at all.
remove_left_([#ee_buffer_line{contents = ContentsFirst}, #ee_buffer_line{line_no = RemoveLineNo, contents = ContentsSecond, eol = Eol}|T], #ee_buffer_coords{line_no = RemoveLineNo, line_offset = 1}) ->
	[#ee_buffer_line{line_no = RemoveLineNo - 1, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|add_to_line_no(T, -1)];
%% A char is being removed.
remove_left_([#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line|T], #ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset}) ->
	{A, [_|B]} = lists:split(RemoveOffset - 2, Contents),
	NewContents = A ++ B,
	[Line#ee_buffer_line{contents = NewContents}|T];
remove_left_([Line|T], RemoveCoords) ->
	[Line|remove_left_(T, RemoveCoords)].

%% We are at the end. No removal should be done.
remove_right_(Lines, #ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset}, LastLineLength) when RemoveLineNo == length(Lines), RemoveOffset == LastLineLength + 1 ->
	Lines;
%% An eol is being removed.
remove_right_([#ee_buffer_line{line_no = RemoveLineNo, contents = ContentsFirst}, #ee_buffer_line{contents = ContentsSecond, eol = Eol}|T], #ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset}, _) when RemoveOffset == length(ContentsFirst) + 1 ->
	[#ee_buffer_line{line_no = RemoveLineNo, contents = ContentsFirst ++ ContentsSecond, eol = Eol}|add_to_line_no(T, -1)];
%% A char is being removed.
remove_right_([#ee_buffer_line{line_no = RemoveLineNo, contents = Contents} = Line|T], #ee_buffer_coords{line_no = RemoveLineNo, line_offset = RemoveOffset}, _) ->
	{A, [_|B]} = lists:split(RemoveOffset - 1, Contents),
	NewContents = A ++ B,
	[Line#ee_buffer_line{contents = NewContents}|T];
remove_right_([Line|T], RemoveCoords, LastLineLength) ->
	[Line|remove_right_(T, RemoveCoords, LastLineLength)].
	
add_to_line_no([], _) ->
	[];
add_to_line_no([#ee_buffer_line{line_no = LineNo} = Line|T], Number) ->
	[Line#ee_buffer_line{line_no = LineNo + Number}|add_to_line_no(T, Number)].

lines_to_string([], AccText) ->
	AccText;
lines_to_string([#ee_buffer_line{contents = Text, eol = Eol}|T], AccText) ->
	lines_to_string(T, AccText ++ Text ++ encode_eol(Eol)).

encode_eol(none) -> [];
encode_eol(eol_lf) -> [?ASCII_LF];
encode_eol(eol_crlf) -> [?ASCII_CR, ?ASCII_LF].
	
