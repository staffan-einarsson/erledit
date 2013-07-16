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
-include("ee_doc_set.hrl").

-record(main_window, {window, status_bar}).
-record(state, {win = #main_window{}, doc_set = #ee_doc_set{}}).

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

handle_call({get_buffer}, _From, #state{doc_set = #ee_doc_set{focus_doc = undefined}} = State) ->
	{reply, {undefined, #ee_caret{}}, State};
handle_call({get_buffer}, _From, #state{doc_set = #ee_doc_set{focus_doc = #ee_doc_view{buffer = Buffer, caret = Caret}}} = State) ->
	%% TODO: Encapsulate doc view in doc set API.
	{reply, {Buffer, Caret}, State};
handle_call(Msg, _From, _State) ->
	erlang:error({bad_call, Msg}).

handle_cast(Msg, _State) ->
	erlang:error({bad_cast, Msg}).

handle_info(#wx{event = #wxClose{}}, #state{win = #main_window{window = Window}} = State) ->
	wxWindow:destroy(Window),
	{stop, normal, State};
handle_info(#wx{event = #wxKey{} = KeyEvent}, State) ->
	{noreply, handle_key(KeyEvent, State)};
%% TODO: replace raw ee_pubsub message with a record.
handle_info({ee_pubsub, {document_inserted, DocPid}, _From}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:add_focus_document_view(DocSet0, DocPid),
	ee_buffer_server:add_subscriber(DocPid, self()),
	{ok, Buffer} = ee_buffer_server:get_buffer(DocPid),
	%% TODO: Look intoif this is efficient (looking up the doc view by pid again)
	{ok, DocSet2} = ee_doc_set:update_document_view_buffer(DocSet1, DocPid, Buffer),
	wxFrame:refresh(Window),
	{noreply, State#state{doc_set = DocSet2}};
handle_info({ee_pubsub, {document_deleted, DocPid}, _From}, #state{doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:remove_document_view(DocSet0, DocPid),
	{noreply, State#state{doc_set = DocSet1}};
handle_info({ee_pubsub, {buffer_update, Buffer}, DocPid}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:update_document_view_buffer(DocSet0, DocPid, Buffer),
	wxFrame:refresh(Window),
	{noreply, State#state{doc_set = DocSet1}};
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

handle_key(#wxKey{type = char, keyCode = ?WXK_F1}, State) ->
	ee_document_sup:open_document("readme"),
	State;
handle_key(#wxKey{type = char, keyCode = ?WXK_LEFT}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_left_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_RIGHT}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_right_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_UP}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_up_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_DOWN}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_down_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_HOME}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_to_line_start_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_END}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_to_line_end_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_PAGEUP}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_up_one_page_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_PAGEDOWN}, #state{win = #main_window{window = Window}, doc_set = DocSet0} = State) ->
	{ok, DocSet1} = ee_doc_set:move_caret_down_one_page_in_focus_doc(DocSet0),
	wxFrame:refresh(Window),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_RETURN}, #state{doc_set = DocSet0} = State) ->
	%% TODO: Refactor so that doc_set internals are hidden.
	#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid, buffer = Buffer, caret = #ee_caret{line_no = LineNo} = Caret0} = FocusDoc} = DocSet0,
	ee_buffer_server:insert_eol(DocPid, ee_caret:caret_to_buffer_coords(Caret0, Buffer)),
	%% TODO: Avoid changing caret directly. Waiting for #29.
	Caret1 = Caret0#ee_caret{line_no = LineNo + 1, col_no = 1},
	DocSet1 = DocSet0#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}},
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_BACK}, #state{doc_set = DocSet0} = State) ->
	%% TODO: Refactor so that doc_set internals are hidden.
	#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid, buffer = Buffer, caret = Caret}} = DocSet0,
	ee_buffer_server:remove_left(DocPid, ee_caret:caret_to_buffer_coords(Caret, Buffer)),
	{ok, DocSet1} = ee_doc_set:move_caret_left_in_focus_doc(DocSet0),
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = ?WXK_DELETE}, #state{doc_set = DocSet} = State) ->
	%% TODO: Refactor so that doc_set internals are hidden.
	#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid, buffer = Buffer, caret = Caret}} = DocSet,
	ee_buffer_server:remove_right(DocPid, ee_caret:caret_to_buffer_coords(Caret, Buffer)),
	State;	
handle_key(#wxKey{type = char, keyCode = ?WXK_TAB}, #state{doc_set = DocSet0} = State) ->
	%% TODO: Refactor so that doc_set internals are hidden.
	#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid, buffer = Buffer, caret = #ee_caret{col_no = ColNo} = Caret0} = FocusDoc} = DocSet0,
	ee_buffer_server:insert_text(DocPid, [?WXK_TAB], ee_caret:caret_to_buffer_coords(Caret0, Buffer)),
	%% TODO: Simplify this caret movement.
	Caret1 = ee_caret:move_to(Caret0#ee_caret{col_no = ColNo + 4}, Buffer),
	DocSet1 = DocSet0#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}},
	State#state{doc_set = DocSet1};
handle_key(#wxKey{type = char, keyCode = KeyCode}, #state{doc_set = DocSet0} = State) when KeyCode > 31, KeyCode < 256 ->
	%% TODO: Refactor so that doc_set internals are hidden.
	#ee_doc_set{focus_doc = #ee_doc_view{pid = DocPid, buffer = Buffer, caret = #ee_caret{col_no = ColNo} = Caret0} = FocusDoc} = DocSet0,
	ee_buffer_server:insert_text(DocPid, [KeyCode], ee_caret:caret_to_buffer_coords(Caret0, Buffer)),
	%% TODO: Simplify this caret movement.
	Caret1 = ee_caret:move_to(Caret0#ee_caret{col_no = ColNo + 1}, Buffer),
	DocSet1 = DocSet0#ee_doc_set{focus_doc = FocusDoc#ee_doc_view{caret = Caret1}},
	State#state{doc_set = DocSet1};
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

draw_buffer(Window, undefined, _) ->
	DC = wxPaintDC:new(Window),
	wxPaintDC:destroy(DC),
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
