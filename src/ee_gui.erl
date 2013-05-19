%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_gui).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([handle_paint/2]).

%-compile(export_all).

-include("ee_global.hrl").
-include_lib("wx/include/wx.hrl").
-include("ee_document.hrl").
-include("ee_buffer.hrl").

-record(main_window, {window, status_bar}).
-record(state, {win = #main_window{}, buffer, caret = #caret{}}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	gen_server:start_link({local, gui}, ?MODULE, [], [
		%{debug, [trace]}
		]).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	wx:new(),
	MainWindow = create_window(),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	State = #state{win = MainWindow},
	io:format("~p Original state is ~p~n", [self(), State]),
	{ok, State}.

terminate(Reason, State) ->
	io:format("~p Terminate: Reason: ~p~nState: ~p~n", [self(), Reason, State]),
	wx:destroy(),
	ok.

handle_call({get_buffer}, _From, #state{buffer = Buffer, caret = Caret} = State) ->
	{reply, {Buffer, Caret}, State};
handle_call(Msg, _From, State) ->
	io:format("~p Got Call ~p~n", [self(), Msg]),
	{reply, ok, State}.

handle_cast({buffer, Buffer}, #state{win = #main_window{window = Window}} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{buffer = Buffer}};
handle_cast(Msg, State) ->
	io:format("~p Got Cast ~p~n", [self(), Msg]),
	{noreply, State}.

handle_info(#wx{event = #wxClose{}}, #state{win = #main_window{window = Window}} = State) ->
	wxWindow:destroy(Window),
	{stop, normal, State};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_LEFT}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_left(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_RIGHT}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_right(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_UP}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_up(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_DOWN}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_down(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_HOME}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_to_beginning_of_line(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_END}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_to_end_of_line(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_PAGEUP}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_up_one_page(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_PAGEDOWN}}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret = move_caret_down_one_page(Caret, Buffer)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_RETURN}}, #state{caret = #caret{line = Line} = Caret} = State) ->
	gen_server:cast(data_buffer, {eol, Caret}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State#state{caret = Caret#caret{line = Line + 1, column = 1}}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_BACK}}, #state{buffer = Buffer, caret = Caret} = State) ->
	gen_server:cast(data_buffer, {delete_left, Caret}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State#state{caret = move_caret_left(Caret, Buffer)}};	
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_DELETE}}, #state{caret = Caret} = State) ->
	gen_server:cast(data_buffer, {delete_right, Caret}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State};	
handle_info(#wx{event = #wxKey{type = char, uniChar = Char}}, #state{caret = #caret{column = Column} = Caret} = State) ->
	%% Send message to data buffer to update.
	gen_server:cast(data_buffer, {char, [Char], Caret}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State#state{caret = Caret#caret{column = Column + 1}}};
handle_info(Msg, State) ->
	io:format("~p Got Info ~p~n", [self(), Msg]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

create_window() ->
	%% Create the window.
	Window = wxFrame:new(wx:null(), -1, "erledit", [{size, {600, 400}}]),
	StatusBar = wxFrame:createStatusBar(Window),
	ok = wxStatusBar:setStatusText(StatusBar, ""),

	%% Subscribe to events.
	wxFrame:connect(Window, close_window),
	wxFrame:connect(Window, char),
	wxFrame:connect(Window, paint
		, [{callback, fun handle_paint/2}]
		),

	%% Show window.
	wxWindow:show(Window),
	#main_window{window = Window, status_bar = StatusBar}.

handle_paint(#wx{obj = Window}, _WxObject) ->
	{Buffer, Caret} = gen_server:call(gui, {get_buffer}),
	draw_buffer(Window, Buffer, Caret),
	ok.

draw_buffer(Window, Buffer, Caret) ->
	DC = wxPaintDC:new(Window),
	ok = wxDC:setBackground(DC, ?wxWHITE_BRUSH),
	Font = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
	ok = wxDC:setFont(DC, Font),
	ok = wxDC:clear(DC),
	ok = draw_buffer_lines(DC, Buffer),
	ok = draw_caret(DC, Buffer, Caret),
	wxFont:destroy(Font),
	wxPaintDC:destroy(DC),
	ok.

draw_buffer_lines(DC, Buffer) ->
	ee_buffer:foreach_line(Buffer,
		fun(Line) -> ok = draw_buffer_line(DC, ee_buffer:get_line_contents(Line), [], 0, (ee_buffer:get_line_number(Line) - 1) * 20) end
		).

draw_buffer_line(DC, [], LiteralCharsRev, XStart, Y) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	ok = wxDC:drawText(DC, LiteralChars, {XStart, Y});
draw_buffer_line(DC, [?ASCII_TAB|T], LiteralCharsRev, XStart, Y) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	{W, _} = wxDC:getTextExtent(DC, LiteralChars),
	ok = wxDC:drawText(DC, LiteralChars, {XStart, Y}),
	draw_buffer_line(DC, T, [], (((XStart + W) div 50) + 1) * 50, Y);
draw_buffer_line(DC, [Char|T], LiteralCharsRev, XStart, Y) ->
	draw_buffer_line(DC, T, [Char|LiteralCharsRev], XStart, Y).

draw_caret(DC, Buffer, #caret{line = LineNo, column = Column}) ->
	Line = ee_buffer:get_line(Buffer, LineNo),
	draw_caret_on_line(DC, lists:sublist(ee_buffer:get_line_contents(Line), Column - 1), [], 0, (LineNo - 1) * 20).

draw_caret_on_line(DC, [], LiteralCharsRev, XStart, Y) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	{W, H} = wxDC:getTextExtent(DC, LiteralChars),
	ok = wxDC:drawLine(DC, {XStart + W, Y}, {XStart + W, Y + H});
draw_caret_on_line(DC, [?ASCII_TAB|T], LiteralCharsRev, XStart, Y) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	{W, _} = wxDC:getTextExtent(DC, LiteralChars),
	draw_caret_on_line(DC, T, [], (((XStart + W) div 50) + 1) * 50, Y);
draw_caret_on_line(DC, [Char|T], LiteralCharsRev, XStart, Y) ->
	draw_caret_on_line(DC, T, [Char|LiteralCharsRev], XStart, Y).

move_caret_left(#caret{line = 1, column = 1} = Caret, _Buffer) ->
	Caret;
move_caret_left(#caret{line = LineNo, column = 1} = Caret, Buffer) ->
	Caret#caret{line = LineNo - 1, column = ee_buffer:get_line_length(Buffer, LineNo - 1) + 1};
move_caret_left(#caret{column = Column} = Caret, _Buffer) ->
	Caret#caret{column = Column - 1}.

move_caret_right(#caret{line = LineNo, column = Column} = Caret, Buffer)  ->
	case Column =< ee_buffer:get_line_length(Buffer, LineNo) of
		true ->
			Caret#caret{column = Column + 1};
		_ ->
			move_caret_right_to_next_line(Caret, ee_buffer:get_num_lines(Buffer))
	end.

move_caret_right_to_next_line(#caret{line = LineNo} = Caret, NumLines) when LineNo < NumLines ->
	Caret#caret{line = LineNo + 1, column = 1};
move_caret_right_to_next_line(Caret, _NumLines) ->
	Caret.
	
move_caret_up(Caret, Buffer) ->
	move_caret_up(Caret, 1, Buffer).
	
move_caret_up(#caret{line = LineNo} = Caret, NumLinesToMove, _Buffer) when LineNo =< NumLinesToMove ->
	Caret#caret{line = 1, column = 1};
move_caret_up(#caret{line = LineNo, column = Column} = Caret, NumLinesToMove, Buffer) ->
	TargetLineLength = ee_buffer:get_line_length(Buffer, LineNo - NumLinesToMove),
	case Column >= TargetLineLength of
		true ->
			Caret#caret{line = LineNo - NumLinesToMove, column = TargetLineLength + 1};
		_ ->
			Caret#caret{line = LineNo - NumLinesToMove}
	end.

move_caret_down(Caret, Buffer) ->
	move_caret_down(Caret, 1, Buffer).

move_caret_down(#caret{line = LineNo} = Caret, NumLinesToMove, Buffer) ->
	NumLines = ee_buffer:get_num_lines(Buffer),
	case LineNo + NumLinesToMove > NumLines of
		true ->
			Caret#caret{line = NumLines, column = ee_buffer:get_line_length(Buffer, NumLines) + 1};
		_ ->
			move_caret_down_and_truncate(Caret, NumLinesToMove, ee_buffer:get_line_length(Buffer, LineNo + NumLinesToMove))
	end.

move_caret_down_and_truncate(#caret{line = LineNo, column = Column} = Caret, NumLinesToMove, NextLineLength) when Column > NextLineLength + 1 ->
	Caret#caret{line = LineNo + NumLinesToMove, column = NextLineLength + 1};
move_caret_down_and_truncate(#caret{line = LineNo} = Caret, NumLinesToMove, _NextLineLength) ->
	Caret#caret{line = LineNo + NumLinesToMove}.

move_caret_to_beginning_of_line(#caret{} = Caret, _) ->
	Caret#caret{column = 1}.

move_caret_to_end_of_line(#caret{line = LineNo} = Caret, Buffer) ->
	Caret#caret{column = ee_buffer:get_line_length(Buffer, LineNo) + 1}.

move_caret_up_one_page(Caret, Buffer) ->
	%% Let's pretend one page is 10 lines for now.
	move_caret_up(Caret, 10, Buffer).
	
move_caret_down_one_page(Caret, Buffer) ->
	%% Let's pretend one page is 10 lines for now.
	move_caret_down(Caret, 10, Buffer).