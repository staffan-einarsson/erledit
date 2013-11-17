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

-module(ee_caret).

%% API
-export(
		[
		move_left/2,
		move_right/2,
		move_up/2,
		move_down/2,
		move_to_beginning_of_line/2,
		move_to_end_of_line/2,
		move_up_one_page/2,
		move_down_one_page/2,
		move_to/2,
		caret_to_buffer_coords/2,
		buffer_coords_to_caret/2
		]
	).

-include_lib("eunit/include/eunit.hrl").
-include("ee_global.hrl").
-include("ee_buffer_coords.hrl").
-include("ee_caret.hrl").

%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

move_left(
		#ee_caret{} = Caret,
		Buffer
	)
	->
		move_horizontally(Caret, -1, Buffer).

%% --------------------------------------------------------------------

move_left_test_()
	->
		TestBufferEmpty = ee_buffer:new(),
		TestBuffer = ee_buffer:new(
				[
				ee_buffer_line:new(1, "Hello", eol_lf),
				ee_buffer_line:new(2, "World", eol_lf),
				ee_buffer_line:new(3, "How are you", eol_lf),
				ee_buffer_line:new(4, "doing??", eol_lf),
				ee_buffer_line:new(5, "", none)
				]
			),
		[
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 1},
			move_left(
				#ee_caret{line_no = 1, col_no = 1},
				TestBufferEmpty
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 1},
			move_left(
				#ee_caret{line_no = 1, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 3},
			move_left(
				#ee_caret{line_no = 2, col_no = 4},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 6},
			move_left(
				#ee_caret{line_no = 2, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 7},
			move_left(
				#ee_caret{line_no = 4, col_no = 9},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 8},
			move_left(
				#ee_caret{line_no = 5, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 8},
			move_left(
				#ee_caret{line_no = 5, col_no = 2},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 8},
			move_left(
				#ee_caret{line_no = 6, col_no = 1},
				TestBuffer
				)
			)
		].

%% --------------------------------------------------------------------

move_right(
		#ee_caret{} = Caret,
		Buffer
	)
	->
		move_horizontally(Caret, 1, Buffer).

%% --------------------------------------------------------------------

move_right_test_()
	->
		TestBufferEmpty = ee_buffer:new(),
		TestBuffer = ee_buffer:new(
				[
				ee_buffer_line:new(1, "Hello", eol_lf),
				ee_buffer_line:new(2, "World", eol_lf),
				ee_buffer_line:new(3, "How are you", eol_lf),
				ee_buffer_line:new(4, "doing??", eol_lf),
				ee_buffer_line:new(5, "", none)
				]
			),
		[
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 1},
			move_right(
				#ee_caret{line_no = 1, col_no = 1},
				TestBufferEmpty
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 4},
			move_right(
				#ee_caret{line_no = 2, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 1},
			move_right(
				#ee_caret{line_no = 1, col_no = 6},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_right(
				#ee_caret{line_no = 4, col_no = 8},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_right(
				#ee_caret{line_no = 4, col_no = 9},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_right(
				#ee_caret{line_no = 5, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_right(
				#ee_caret{line_no = 5, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_right(
				#ee_caret{line_no = 6, col_no = 1},
				TestBuffer
				)
			)
		].

%% --------------------------------------------------------------------

move_up(
		Caret,
		Buffer
	)
	->
		AdjustedCaret = #ee_caret{line_no = AdjustedLineNo} =
			buffer_coords_to_caret(caret_to_buffer_coords(Caret, Buffer), Buffer),
		move_to(AdjustedCaret#ee_caret{line_no = AdjustedLineNo - 1}, Buffer).

%% --------------------------------------------------------------------

move_up_test_()
	->
		TestBufferEmpty = ee_buffer:new(),
		TestBuffer = ee_buffer:new(
				[
				ee_buffer_line:new(1, "Hello", eol_lf),
				ee_buffer_line:new(2, "World", eol_lf),
				ee_buffer_line:new(3, "How are you", eol_lf),
				ee_buffer_line:new(4, "doing??", eol_lf),
				ee_buffer_line:new(5, "", none)
				]
			),
		[
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 1},
			move_up(
				#ee_caret{line_no = 1, col_no = 1},
				TestBufferEmpty
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 1},
			move_up(
				#ee_caret{line_no = 1, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 3},
			move_up(
				#ee_caret{line_no = 2, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 6},
			move_up(
				#ee_caret{line_no = 3, col_no = 12},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 6},
			move_up(
				#ee_caret{line_no = 3, col_no = 15},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 1},
			move_up(
				#ee_caret{line_no = 5, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 1},
			move_up(
				#ee_caret{line_no = 6, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 1},
			move_up(
				#ee_caret{line_no = 6, col_no = 6},
				TestBuffer
				)
			)
		].

%% --------------------------------------------------------------------

move_down(
		Caret,
		Buffer
	)
	->
		AdjustedCaret = #ee_caret{line_no = AdjustedLineNo} =
			buffer_coords_to_caret(caret_to_buffer_coords(Caret, Buffer), Buffer),
		move_to(AdjustedCaret#ee_caret{line_no = AdjustedLineNo + 1}, Buffer).

%% --------------------------------------------------------------------

move_down_test_()
	->
		TestBufferEmpty = ee_buffer:new(),
		TestBuffer = ee_buffer:new(
				[
				ee_buffer_line:new(1, "Hello", eol_lf),
				ee_buffer_line:new(2, "World", eol_lf),
				ee_buffer_line:new(3, "How are you", eol_lf),
				ee_buffer_line:new(4, "doing??", eol_lf),
				ee_buffer_line:new(5, "", none)
				]
			),
		[
		?_assertEqual(
			#ee_caret{line_no = 1, col_no = 1},
			move_down(
				#ee_caret{line_no = 1, col_no = 1},
				TestBufferEmpty
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 1},
			move_down(
				#ee_caret{line_no = 0, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 1},
			move_down(
				#ee_caret{line_no = 0, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 2, col_no = 3},
			move_down(
				#ee_caret{line_no = 1, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 3, col_no = 6},
			move_down(
				#ee_caret{line_no = 2, col_no = 10},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 8},
			move_down(
				#ee_caret{line_no = 3, col_no = 12},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 4, col_no = 8},
			move_down(
				#ee_caret{line_no = 3, col_no = 15},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_down(
				#ee_caret{line_no = 4, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_down(
				#ee_caret{line_no = 4, col_no = 3},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_down(
				#ee_caret{line_no = 5, col_no = 1},
				TestBuffer
				)
			),
		?_assertEqual(
			#ee_caret{line_no = 5, col_no = 1},
			move_down(
				#ee_caret{line_no = 5, col_no = 5},
				TestBuffer
				)
			)
		].

%% --------------------------------------------------------------------

move_up_one_page(
		#ee_caret{line_no = LineNo} = Caret,
		Buffer
	)
	->
		%% Let's pretend one page is 10 lines for now.
		move_to(Caret#ee_caret{line_no = LineNo - 10}, Buffer).
	
%% --------------------------------------------------------------------

move_down_one_page(
		#ee_caret{line_no = LineNo} = Caret,
		Buffer
	)
	->
		%% Let's pretend one page is 10 lines for now.
		move_to(Caret#ee_caret{line_no = LineNo + 10}, Buffer).

%% --------------------------------------------------------------------

move_horizontally(
		Caret,
		Amount,
		Buffer
	)
	->
		BufferCoords = #ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
		CurrentLineLength = ee_buffer:get_line_length(Buffer, LineNo),
		PreviousLineLength = (catch ee_buffer:get_line_length(Buffer, LineNo - 1)),
		LastLineNo = ee_buffer:get_num_lines(Buffer),
		move_horizontally(Caret, BufferCoords, Amount, Buffer, CurrentLineLength, PreviousLineLength, LastLineNo).

%% --------------------------------------------------------------------

%% Move left more than one line AND this is the first line -> clamp at beginning of line
move_horizontally(
		Caret,
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Amount,
		Buffer,
		_CurrentLineLength,
		_PreviousLineLength,
		_LastLineNo
	)
		when Amount < 1 - LineOffset, LineNo == 1
	->
		move_to_beginning_of_line(Caret, Buffer);
%% Move right more than one line AND this is the last line -> clamp at end of line
move_horizontally(
		Caret,
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Amount,
		Buffer,
		CurrentLineLength,
		_PreviousLineLength,
		LastLineNo
	)
		when Amount > CurrentLineLength + 1 - LineOffset, LineNo == LastLineNo
	->
		move_to_end_of_line(Caret, Buffer);
%% Move left more than one line -> subtract offset and recurse on prev line
move_horizontally(
		Caret,
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Amount,
		Buffer,
		_CurrentLineLength,
		PreviousLineLength,
		LastLineNo
	)
		when Amount < 1 - LineOffset
	->
		move_horizontally(Caret, ee_buffer_coords:new(LineNo - 1, PreviousLineLength + 1), Amount + LineOffset, Buffer, PreviousLineLength, catch ee_buffer:get_line_length(Buffer, LineNo - 2), LastLineNo);
%% Move right more than one line -> subtract offset and recurse on next line
move_horizontally(
		Caret,
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Amount,
		Buffer,
		CurrentLineLength,
		_PreviousLineLength,
		LastLineNo
	)
		when Amount > CurrentLineLength + 1 - LineOffset
	->
		move_horizontally(Caret, ee_buffer_coords:new(LineNo + 1, 1), Amount - (CurrentLineLength + 1 - LineOffset) - 1, Buffer, ee_buffer:get_line_length(Buffer, LineNo + 1), CurrentLineLength, LastLineNo);
%% Move within current line -> Update coords and caret
move_horizontally(
		_Caret,
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Amount,
		Buffer,
		_CurrentLineLength,
		_PreviousLineLength,
		_LastLineNo
	)
	->
		buffer_coords_to_caret(ee_buffer_coords:new(LineNo, LineOffset + Amount), Buffer).
	
%% --------------------------------------------------------------------

move_to_beginning_of_line(
		#ee_caret{} = Caret,
		Buffer
	)
	->
		#ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
		buffer_coords_to_caret(ee_buffer_coords:new(LineNo, 1), Buffer).

%% --------------------------------------------------------------------

move_to_end_of_line(
		#ee_caret{} = Caret,
		Buffer
	)
	->
		#ee_buffer_coords{line_no = LineNo} = caret_to_buffer_coords(Caret, Buffer),
		buffer_coords_to_caret(ee_buffer_coords:new(LineNo, ee_buffer:get_line_length(Buffer, LineNo) + 1), Buffer).

%% --------------------------------------------------------------------

move_to(
		Caret,
		Buffer
	)
	->
		Coords = #ee_buffer_coords{line_no = ActualLineNo} = caret_to_buffer_coords(Caret, Buffer),
		move_to_buffer_coords(
			Coords,
			Buffer,
			ee_buffer:get_line_length(Buffer, ActualLineNo),
			ee_buffer:get_num_lines(Buffer)
			).

%% --------------------------------------------------------------------

%% Move to a line before the beginning -> Clamp to first line
move_to_buffer_coords(
		#ee_buffer_coords{line_no = LineNo},
		Buffer,
		_LineLength,
		_NumLines
	)
		when LineNo < 1
	->
		buffer_coords_to_caret(ee_buffer_coords:new(1, 1), Buffer);
%% Move to a line after the end -> Clamp to last line
move_to_buffer_coords(
		#ee_buffer_coords{line_no = LineNo},
		Buffer,
		_LineLength,
		NumLines
	)
		when LineNo > NumLines
	->
		buffer_coords_to_caret(ee_buffer_coords:new(NumLines, ee_buffer:get_line_length(Buffer, NumLines) + 1), Buffer);
%% Move to a line where the given pos is before beginning of line -> Clamp to beginning of line
move_to_buffer_coords(
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Buffer,
		_LineLength,
		_NumLines
	)
		when LineOffset < 1
	->
		buffer_coords_to_caret(ee_buffer_coords:new(LineNo, 1), Buffer);
%% Move to a line where the given pos is after end of line -> Clamp to end of line
move_to_buffer_coords(
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Buffer,
		LineLength,
		_NumLines
	)
		when LineOffset > LineLength + 1
	->
		buffer_coords_to_caret(ee_buffer_coords:new(LineNo, LineLength + 1), Buffer);
%% Move to a line where the given pos is valid
move_to_buffer_coords(
		Coords,
		Buffer,
		_LineLength,
		_NumLines
	)
	->
		buffer_coords_to_caret(Coords, Buffer).
	
%% --------------------------------------------------------------------

caret_to_buffer_coords(
		#ee_caret{line_no = LineNo},
		_
	)
		when LineNo < 1
	->
		ee_buffer_coords:new(1, 1);
caret_to_buffer_coords(
		#ee_caret{line_no = LineNo, col_no = ColNo},
		Buffer
	)
	->
		ActualLineNo = min(LineNo, ee_buffer:get_num_lines(Buffer)),
		LineContents = ee_buffer_line:get_line_contents(ee_buffer:get_line(Buffer, ActualLineNo)),
		%% Get each char on line until colno has been reached.
		Offset = caret_to_buffer_coords_loop(LineContents, 1, ColNo),
		ee_buffer_coords:new(ActualLineNo, Offset).

%% --------------------------------------------------------------------

caret_to_buffer_coords_loop(
		no_contents,
		_,
		_
	)
	->
		no_contents;
caret_to_buffer_coords_loop(
		[],
		Offset,
		_RemainCols
	)
	->
		Offset;
caret_to_buffer_coords_loop(
		_Chars,
		Offset,
		RemainCols
	)
		when RemainCols =< 1
	->
		Offset;
caret_to_buffer_coords_loop(
		[$\t|T],
		Offset,
		RemainCols
	)
	->
		caret_to_buffer_coords_loop(T, Offset + 1, RemainCols - 4);
caret_to_buffer_coords_loop(
		[_Char|T],
		Offset,
		RemainCols
	)
	->
		caret_to_buffer_coords_loop(T, Offset + 1, RemainCols - 1).
	
%% --------------------------------------------------------------------

buffer_coords_to_caret(
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset},
		Buffer
	)
	->
		LineContents = ee_buffer_line:get_line_contents(ee_buffer:get_line(Buffer, LineNo)),
		%% Get each char on line until colno has been reached.
		ColNo = find_column(LineContents, 1, LineOffset),
		#ee_caret{line_no = LineNo, col_no = ColNo}.

%% --------------------------------------------------------------------

find_column(
		[],
		Cols,
		_RemainOffset
	)
	->
		Cols;
find_column(
		_Chars,
		Cols,
		RemainOffset
	)
		when RemainOffset =< 1
	->
		Cols;
find_column(
		[$\t|T],
		Cols,
		RemainOffset
	)
	->
		find_column(T, Cols + 4, RemainOffset - 1);
find_column(
		[_Char|T],
		Cols,
		RemainOffset
	)
	->
		find_column(T, Cols + 1, RemainOffset - 1).
