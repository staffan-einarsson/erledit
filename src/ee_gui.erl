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
-record(state, {win = #main_window{}, buffer, caret = #ee_caret{}, documents = []}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], [
		%{debug, [trace]}
		]).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	ee_document_controller:add_subscriber(self()),
	{ok, #state{}, 0}.

terminate(_Reason, _State) ->
	wx:destroy(),
	ok.

handle_call({get_buffer}, _From, #state{buffer = Buffer, caret = Caret} = State) ->
	{reply, {Buffer, Caret}, State};
handle_call(Msg, _From, _State) ->
	erlang:error({bad_call, Msg}).

handle_cast({buffer, Buffer}, #state{win = #main_window{window = Window}} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{buffer = Buffer}};
handle_cast(Msg, _State) ->
	erlang:error({bad_cast, Msg}).

handle_info(#wx{event = #wxClose{}}, #state{win = #main_window{window = Window}} = State) ->
	wxWindow:destroy(Window),
	{stop, normal, State};
handle_info(#wx{event = #wxKey{} = KeyEvent}, State) ->
	{noreply, handle_key(KeyEvent, State)};
handle_info({ee_pubsub, {document_inserted, DocPid}, _From}, #state{documents = Documents} = State) ->
	gen_server:cast(DocPid, {get_buffer, self()}),
	{noreply, State#state{documents = [DocPid|Documents]}};
handle_info({ee_pubsub, {document_deleted, DocPid}, _From}, #state{documents = Documents} = State) ->
	{noreply, State#state{documents = lists:delete(DocPid, Documents)}};
handle_info(timeout, State) ->
	wx:new(),
	{noreply, State#state{win = create_window()}};
handle_info(Msg, _State) ->
	erlang:error({bad_info, Msg}).

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
handle_key(#wxKey{type = char, keyCode = ?WXK_RETURN}, #state{buffer = Buffer, caret = #ee_caret{line_no = LineNo} = Caret, documents = [Document]} = State) ->
	gen_server:cast(Document, {eol, ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(Document, {get_buffer, self()}),
	%% TODO: Avoid changing caret directly. Waiting for #29.
	State#state{caret = Caret#ee_caret{line_no = LineNo + 1, col_no = 1}};
handle_key(#wxKey{type = char, keyCode = ?WXK_BACK}, #state{buffer = Buffer, caret = Caret, documents = [Document]} = State) ->
	gen_server:cast(Document, {delete_left, ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(Document, {get_buffer, self()}),
	State#state{caret = ee_caret:move_left(Caret, Buffer)};	
handle_key(#wxKey{type = char, keyCode = ?WXK_DELETE}, #state{buffer = Buffer, caret = Caret, documents = [Document]} = State) ->
	gen_server:cast(Document, {delete_right, ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(Document, {get_buffer, self()}),
	State;	
handle_key(#wxKey{type = char, keyCode = ?WXK_TAB, uniChar = Char}, #state{buffer = Buffer, caret = #ee_caret{col_no = ColNo} = Caret, documents = [Document]} = State) ->
	%% Send message to data buffer to update.
	gen_server:cast(Document, {char, [Char], ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(Document, {get_buffer, self()}),
	State#state{caret = ee_caret:move_to(Caret#ee_caret{col_no = ColNo + 4}, Buffer)};
handle_key(#wxKey{type = char, keyCode = KeyCode}, #state{buffer = Buffer, caret = #ee_caret{col_no = ColNo} = Caret, documents = [Document]} = State) when KeyCode > 31, KeyCode < 256 ->
	%% Send message to data buffer to update.
	gen_server:cast(Document, {char, [KeyCode], ee_caret:caret_to_buffer_coords(Caret, Buffer)}),
	gen_server:cast(Document, {get_buffer, self()}),
	State#state{caret = ee_caret:move_to(Caret#ee_caret{col_no = ColNo + 1}, Buffer)};
handle_key(#wxKey{type = char, keyCode = KeyCode}, State) ->
	io:format("Ignored key: ~p~n", [KeyCode]),
	State.

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
	{Buffer, Caret} = gen_server:call(?MODULE, {get_buffer}),
	draw_buffer(Window, Buffer, Caret),
	ok.

draw_buffer(_, undefined, _) ->
	ok;
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
