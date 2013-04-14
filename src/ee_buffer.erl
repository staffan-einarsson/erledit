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
	insert_eol/3]).

-include("ee_buffer.hrl").
-ifdef(debug).
-define(dbg_print(Term), erlang:apply(fun() -> Val = Term, io:format("dbg_print: ~p, ~p: ~p~n", [?FILE, ?LINE, Val]), Val end, [])).
-else.
-define(dbg_print(Term), Term).
-endif.

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
	[];
insert_eol_([#ee_buffer_line{line_no = LineNo} = Line|T], CaretLineNo, CaretColNo) when LineNo > CaretLineNo ->
	[Line#ee_buffer_line{line_no = LineNo + 1}|insert_eol_(T, CaretLineNo, CaretColNo)];
insert_eol_([#ee_buffer_line{line_no = CaretLineNo, contents = Contents, eol = Eol} = Line|T], CaretLineNo, CaretColNo) ->
	{A, B} = lists:split(CaretColNo, Contents),
	[Line#ee_buffer_line{contents = A, eol = eol_lf}, Line#ee_buffer_line{line_no = CaretLineNo + 1, contents = B, eol = Eol}|insert_eol_(T, CaretLineNo, CaretColNo)];
insert_eol_([#ee_buffer_line{} = Line|T], CaretLineNo, CaretColNo) ->
	[Line|insert_eol_(T, CaretLineNo, CaretColNo)].
