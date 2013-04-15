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
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_RETURN}}, #state{caret = #caret{line = Line} = Caret} = State) ->
	gen_server:cast(data_buffer, {eol, Caret}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State#state{caret = Caret#caret{line = Line + 1, column = 0}}};
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
		fun(Line) -> ok = draw_buffer_line(DC, ee_buffer:get_line_contents(Line), [], 0, ee_buffer:get_line_number(Line) * 20) end
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
	Line = ee_buffer:get_line(Buffer, LineNo + 1),
	draw_caret_on_line(DC, lists:sublist(ee_buffer:get_line_contents(Line), Column), [], 0, ee_buffer:get_line_number(Line) * 20).

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

move_caret_left(#caret{line = 0, column = 0} = Caret, _Buffer) ->
	Caret;
move_caret_left(#caret{line = LineNo, column = 0} = Caret, Buffer) ->
	Caret#caret{line = LineNo - 1, column = ee_buffer:get_line_length(Buffer, LineNo)};
move_caret_left(#caret{column = Column} = Caret, _Buffer) ->
	Caret#caret{column = Column - 1}.

move_caret_right(#caret{line = LineNo, column = Column} = Caret, Buffer)  ->
	case Column < ee_buffer:get_line_length(Buffer, LineNo + 1) of
		true ->
			Caret#caret{column = Column + 1};
		_ ->
			move_caret_right_to_next_line(Caret, ee_buffer:get_num_lines(Buffer))
	end.

move_caret_right_to_next_line(#caret{line = Line} = Caret, NumLines) when Line < NumLines - 1 ->
	Caret#caret{line = Line + 1, column = 0};
move_caret_right_to_next_line(Caret, _NumLines) ->
	Caret.
	
move_caret_up(#caret{line = 0} = Caret, _Buffer) ->
	Caret#caret{column = 0};
move_caret_up(#caret{line = LineNo, column = Column} = Caret, Buffer) ->
	PreviousLineLength = ee_buffer:get_line_length(Buffer, LineNo),
	case Column > PreviousLineLength of
		true ->
			Caret#caret{line = LineNo - 1, column = PreviousLineLength};
		_ ->
			Caret#caret{line = LineNo - 1}
	end.

move_caret_down(#caret{line = LineNo} = Caret, Buffer) ->
	case LineNo =:= ee_buffer:get_num_lines(Buffer) - 1 of
		true ->
			Caret#caret{column = ee_buffer:get_line_length(Buffer, LineNo + 1)};
		_ ->
			move_caret_down_to_next_line(Caret, ee_buffer:get_line_length(Buffer, LineNo + 2))
	end.

move_caret_down_to_next_line(#caret{line = Line, column = Column} = Caret, NextLineLength) when Column > NextLineLength ->
	Caret#caret{line = Line + 1, column = NextLineLength};
move_caret_down_to_next_line(#caret{line = Line} = Caret, _NextLineLength) ->
	Caret#caret{line = Line + 1}.
