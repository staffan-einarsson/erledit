%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_caret).

%% API
-export([
	move_left/2,
	move_right/2,
	move_up/2,
	move_up/3,
	move_down/2,
	move_down/3,
	move_to_beginning_of_line/2,
	move_to_end_of_line/2,
	move_up_one_page/2,
	move_down_one_page/2,
	caret_to_buffer_coords/2,
	buffer_coords_to_caret/2
	]).

%-compile(export_all).

-include("ee_global.hrl").
-include("ee_buffer.hrl").
-include("ee_caret.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% ===================================================================
%% Internal functions
%% ===================================================================

move_left(#ee_caret{line = 1, column = 1} = Caret, _Buffer) ->
	Caret;
move_left(#ee_caret{line = LineNo, column = 1}, Buffer) ->
	Coords = ee_buffer:new_buffer_coords(LineNo - 1, ee_buffer:get_line_length(Buffer, LineNo - 1) + 1),
	buffer_coords_to_caret(Coords, Buffer);
move_left(#ee_caret{} = Caret, Buffer) ->
	#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset} = caret_to_buffer_coords(Caret, Buffer),
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, LineOffset - 1), Buffer).

move_right(#ee_caret{} = Caret, Buffer) ->
	#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset} = caret_to_buffer_coords(Caret, Buffer),
	case LineOffset =< ee_buffer:get_line_length(Buffer, LineNo) of
		true ->
			buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, LineOffset + 1), Buffer);
		_ ->
			move_right_to_next_line(Caret, ee_buffer:get_num_lines(Buffer))
	end.

move_right_to_next_line(#ee_caret{line = LineNo} = Caret, NumLines) when LineNo < NumLines ->
	Caret#ee_caret{line = LineNo + 1, column = 1};
move_right_to_next_line(Caret, _NumLines) ->
	Caret.
	
move_up(Caret, Buffer) ->
	move_up(Caret, 1, Buffer).
	
move_up(#ee_caret{line = LineNo} = Caret, NumLinesToMove, _Buffer) when LineNo =< NumLinesToMove ->
	Caret#ee_caret{line = 1, column = 1};
move_up(#ee_caret{line = LineNo} = Caret, NumLinesToMove, Buffer) ->
	buffer_coords_to_caret(caret_to_buffer_coords(Caret#ee_caret{line = LineNo - NumLinesToMove}, Buffer), Buffer).

move_down(Caret, Buffer) ->
	move_down(Caret, 1, Buffer).

move_down(#ee_caret{line = LineNo} = Caret, NumLinesToMove, Buffer) ->
	NumLines = ee_buffer:get_num_lines(Buffer),
	case LineNo + NumLinesToMove > NumLines of
		true ->
			Coords = ee_buffer:new_buffer_coords(NumLines, ee_buffer:get_line_length(Buffer, NumLines) + 1),
			buffer_coords_to_caret(Coords, Buffer);
		_ ->
			Coords = caret_to_buffer_coords(Caret, Buffer),
			move_down_and_truncate(Caret, Buffer, NumLinesToMove, Coords, ee_buffer:get_line_length(Buffer, LineNo + NumLinesToMove))
	end.

move_down_and_truncate(#ee_caret{line = LineNo} = Caret, Buffer, NumLinesToMove, _Buffer, _NextLineLength) ->
	buffer_coords_to_caret(caret_to_buffer_coords(Caret#ee_caret{line = LineNo + NumLinesToMove}, Buffer), Buffer).

move_to_beginning_of_line(#ee_caret{} = Caret, _) ->
	Caret#ee_caret{column = 1}.

move_to_end_of_line(#ee_caret{} = Caret, Buffer) ->
	#ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, ee_buffer:get_line_length(Buffer, LineNo) + 1), Buffer).

move_up_one_page(Caret, Buffer) ->
	%% Let's pretend one page is 10 lines for now.
	move_up(Caret, 10, Buffer).
	
move_down_one_page(Caret, Buffer) ->
	%% Let's pretend one page is 10 lines for now.
	move_down(Caret, 10, Buffer).

caret_to_buffer_coords(#ee_caret{line = LineNo, column = ColNo}, Buffer) ->
	LineContents = ee_buffer:get_line_contents(ee_buffer:get_line(Buffer, LineNo)),
	%% Get each char on line until colno has been reached.
	Offset = caret_to_buffer_coords_loop(LineContents, 1, ColNo),
	ee_buffer:new_buffer_coords(LineNo, Offset).

caret_to_buffer_coords_loop([], Offset, _RemainCols) ->
	Offset;
caret_to_buffer_coords_loop(_Chars, Offset, RemainCols) when RemainCols =< 1 ->
	Offset;
caret_to_buffer_coords_loop([$\t|T], Offset, RemainCols) ->
	caret_to_buffer_coords_loop(T, Offset + 1, RemainCols - 4);
caret_to_buffer_coords_loop([_Char|T], Offset, RemainCols) ->
	caret_to_buffer_coords_loop(T, Offset + 1, RemainCols - 1).
	
buffer_coords_to_caret(#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Buffer) ->
	LineContents = ee_buffer:get_line_contents(ee_buffer:get_line(Buffer, LineNo)),
	%% Get each char on line until colno has been reached.
	ColNo = buffer_coords_to_caret_loop(LineContents, 1, LineOffset),
	#ee_caret{line = LineNo, column = ColNo}.

buffer_coords_to_caret_loop([], Cols, _RemainOffset) ->
	Cols;
buffer_coords_to_caret_loop(_Chars, Cols, RemainOffset) when RemainOffset =< 1 ->
	Cols;
buffer_coords_to_caret_loop([$\t|T], Cols, RemainOffset) ->
	buffer_coords_to_caret_loop(T, Cols + 4, RemainOffset - 1);
buffer_coords_to_caret_loop([_Char|T], Cols, RemainOffset) ->
	buffer_coords_to_caret_loop(T, Cols + 1, RemainOffset - 1).
