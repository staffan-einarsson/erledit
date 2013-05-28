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
	move_down/2,
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

move_left(#ee_caret{} = Caret, Buffer) ->
	move_horizontally(Caret, -1, Buffer).

move_right(#ee_caret{} = Caret, Buffer) ->
	move_horizontally(Caret, 1, Buffer).

move_up(#ee_caret{line = LineNo} = Caret, Buffer) ->
	move_to(Caret#ee_caret{line = LineNo - 1}, Buffer).

move_down(#ee_caret{line = LineNo} = Caret, Buffer) ->
	move_to(Caret#ee_caret{line = LineNo + 1}, Buffer).

move_up_one_page(#ee_caret{line = LineNo} = Caret, Buffer) ->
	%% Let's pretend one page is 10 lines for now.
	move_to(Caret#ee_caret{line = LineNo - 10}, Buffer).
	
move_down_one_page(#ee_caret{line = LineNo} = Caret, Buffer) ->
	%% Let's pretend one page is 10 lines for now.
	move_to(Caret#ee_caret{line = LineNo + 10}, Buffer).

move_horizontally(Caret, Amount, Buffer) ->
	BufferCoords = #ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
	move_horizontally(Caret, BufferCoords, Amount, Buffer, ee_buffer:get_line_length(Buffer, LineNo), ee_buffer:get_line_length(Buffer, LineNo - 1), ee_buffer:get_num_lines(Buffer)).

%% Move left more than one line AND this is the first line -> clamp at beginning of line
move_horizontally(Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Amount, Buffer, _CurrentLineLength, _PreviousLineLength, _LastLineNo) when Amount < 1 - LineOffset, LineNo == 1 ->
	move_to_beginning_of_line(Caret, Buffer);
%% Move right more than one line AND this is the last line -> clamp at end of line
move_horizontally(Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Amount, Buffer, CurrentLineLength, _PreviousLineLength, LastLineNo) when Amount > CurrentLineLength + 1 - LineOffset, LineNo == LastLineNo ->
	move_to_end_of_line(Caret, Buffer);
%% Move left more than one line -> subtract offset and recurse on prev line
move_horizontally(Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Amount, Buffer, _CurrentLineLength, PreviousLineLength, LastLineNo) when Amount < 1 - LineOffset ->
	move_horizontally(Caret, ee_buffer:new_buffer_coords(LineNo - 1, PreviousLineLength + 1), Amount + LineOffset, Buffer, PreviousLineLength, ee_buffer:get_line_length(Buffer, LineNo - 2), LastLineNo);
%% Move right more than one line -> subtract offset and recurse on next line
move_horizontally(Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Amount, Buffer, CurrentLineLength, _PreviousLineLength, LastLineNo) when Amount > CurrentLineLength + 1 - LineOffset ->
	move_horizontally(Caret, ee_buffer:new_buffer_coords(LineNo + 1, 1), Amount - (CurrentLineLength + 1 - LineOffset) - 1, Buffer, ee_buffer:get_line_length(Buffer, LineNo + 1), CurrentLineLength, LastLineNo);
%% Move within current line -> Update coords and caret
move_horizontally(_Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Amount, Buffer, _CurrentLineLength, _PreviousLineLength, _LastLineNo) ->
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, LineOffset + Amount), Buffer).
	
move_to_beginning_of_line(#ee_caret{} = Caret, Buffer) ->
	#ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, 1), Buffer).

move_to_end_of_line(#ee_caret{} = Caret, Buffer) ->
	#ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, ee_buffer:get_line_length(Buffer, LineNo) + 1), Buffer).

move_to(#ee_caret{line = LineNo} = Caret, Buffer) ->
	move_to(Caret, caret_to_buffer_coords(Caret, Buffer), Buffer, ee_buffer:get_line_length(Buffer, LineNo), ee_buffer:get_num_lines(Buffer)).

%% Move to a line before the beginning -> Clamp to first line
move_to(_Caret, #ee_buffer_coords{line_no = LineNo}, Buffer, _LineLength, _NumLines) when LineNo < 1 ->
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(1, 1), Buffer);
%% Move to a line after the end -> Clamp to last line
move_to(_Caret, #ee_buffer_coords{line_no = LineNo}, Buffer, _LineLength, NumLines) when LineNo > NumLines ->
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(NumLines, ee_buffer:get_line_length(Buffer, NumLines)), Buffer);
%% Move to a line where the given pos is before beginning of line -> Clamp to beginning of line
move_to(_Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Buffer, _LineLength, _NumLines) when LineOffset < 1 ->
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, 1), Buffer);
%% Move to a line where the given pos is after end of line -> Clamp to end of line
move_to(_Caret, #ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}, Buffer, LineLength, _NumLines) when LineOffset > LineLength + 1 ->
	buffer_coords_to_caret(ee_buffer:new_buffer_coords(LineNo, LineLength + 1), Buffer);
%% Move to a line where the given pos is valid
move_to(Caret, _Coords, _Buffer, _LineLength, _NumLines) ->
	Caret.	
	
caret_to_buffer_coords(#ee_caret{line = LineNo, column = ColNo}, Buffer) ->
	LineContents = ee_buffer:get_line_contents(ee_buffer:get_line(Buffer, LineNo)),
	%% Get each char on line until colno has been reached.
	Offset = caret_to_buffer_coords_loop(LineContents, 1, ColNo),
	ee_buffer:new_buffer_coords(LineNo, Offset).

caret_to_buffer_coords_loop(no_contents, _, _) ->
	no_contents;
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
