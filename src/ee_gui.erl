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
-include("ee_caret.hrl").

-record(main_window, {window, status_bar}).
-record(state, {win = #main_window{}, buffer, caret = #ee_caret{}}).

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
handle_info(#wx{event = #wxKey{} = KeyEvent}, State) ->
	{noreply, handle_key(KeyEvent, State)};
handle_info(Msg, State) ->
	io:format("~p Got Info ~p~n", [self(), Msg]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

handle_key(#wxKey{type = char, keyCode = ?WXK_LEFT}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_left(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_RIGHT}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_right(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_UP}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_up(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_DOWN}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_down(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_HOME}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_to_beginning_of_line(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_END}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_to_end_of_line(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_PAGEUP}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_up_one_page(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_PAGEDOWN}, #state{win = #main_window{window = Window}, buffer = Buffer, caret = Caret} = State) ->
	wxFrame:refresh(Window),
	State#state{caret = ee_caret:move_down_one_page(Caret, Buffer)};
handle_key(#wxKey{type = char, keyCode = ?WXK_RETURN}, #state{buffer = Buffer, caret = #ee_caret{line = Line} = Caret} = State) ->
	gen_server:cast(data_buffer, {eol, ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	%% Don't change caret directly.
	State#state{caret = Caret#ee_caret{line = Line + 1, column = 1}};
handle_key(#wxKey{type = char, keyCode = ?WXK_BACK}, #state{buffer = Buffer, caret = Caret} = State) ->
	gen_server:cast(data_buffer, {delete_left, ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	State#state{caret = ee_caret:move_left(Caret, Buffer)};	
handle_key(#wxKey{type = char, keyCode = ?WXK_DELETE}, #state{buffer = Buffer, caret = Caret} = State) ->
	gen_server:cast(data_buffer, {delete_right, ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	State;	
handle_key(#wxKey{type = char, uniChar = Char}, #state{buffer = Buffer, caret = #ee_caret{column = Column} = Caret} = State) ->
	%% Send message to data buffer to update.
	gen_server:cast(data_buffer, {char, [Char], ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	%% Don't change caret directly.
	State#state{caret = Caret#ee_caret{column = Column + 1}}.

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
	{SpaceWidth, LineHeight} = wxDC:getTextExtent(DC, " "),
	ok = wxDC:clear(DC),
	ok = draw_buffer_lines(DC, Buffer, LineHeight, SpaceWidth),
	ok = draw_caret(DC, Buffer, Caret, LineHeight, SpaceWidth),
	wxFont:destroy(Font),
	wxPaintDC:destroy(DC),
	ok.

draw_buffer_lines(DC, Buffer, LineHeight, SpaceWidth) ->
	ee_buffer:foreach_line(Buffer,
		fun(Line) -> ok = draw_buffer_line(DC, ee_buffer:get_line_contents(Line), [], 0, (ee_buffer:get_line_number(Line) - 1) * LineHeight, SpaceWidth) end
		).

draw_buffer_line(DC, [], LiteralCharsRev, XStart, Y, _) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	ok = wxDC:drawText(DC, LiteralChars, {XStart, Y});
draw_buffer_line(DC, [?ASCII_TAB|T], LiteralCharsRev, XStart, Y, SpaceWidth) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	{W, _} = wxDC:getTextExtent(DC, LiteralChars),
	ok = wxDC:drawText(DC, LiteralChars, {XStart, Y}),
	draw_buffer_line(DC, T, [], (((XStart + W) div (4 * SpaceWidth)) + 1) * (4 * SpaceWidth), Y, SpaceWidth);
draw_buffer_line(DC, [Char|T], LiteralCharsRev, XStart, Y, SpaceWidth) ->
	draw_buffer_line(DC, T, [Char|LiteralCharsRev], XStart, Y, SpaceWidth).

draw_caret(DC, Buffer, Caret, LineHeight, SpaceWidth) ->
	#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset} = ee_caret:caret_to_buffer_coords(Caret, Buffer),
	Line = ee_buffer:get_line(Buffer, LineNo),
	draw_caret_on_line(DC, lists:sublist(ee_buffer:get_line_contents(Line), LineOffset - 1), [], 0, (LineNo - 1) * LineHeight, LineHeight, SpaceWidth).

draw_caret_on_line(DC, [], LiteralCharsRev, XStart, Y, H, _) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	{W, _} = wxDC:getTextExtent(DC, LiteralChars),
	ok = wxDC:drawLine(DC, {XStart + W, Y}, {XStart + W, Y + H});
draw_caret_on_line(DC, [?ASCII_TAB|T], LiteralCharsRev, XStart, Y, H, SpaceWidth) ->
	LiteralChars = lists:reverse(LiteralCharsRev),
	{W, _} = wxDC:getTextExtent(DC, LiteralChars),
	draw_caret_on_line(DC, T, [], (((XStart + W) div (4 * SpaceWidth)) + 1) * (4 * SpaceWidth), Y, H, SpaceWidth);
draw_caret_on_line(DC, [Char|T], LiteralCharsRev, XStart, Y, H, SpaceWidth) ->
	draw_caret_on_line(DC, T, [Char|LiteralCharsRev], XStart, Y, H, SpaceWidth).
