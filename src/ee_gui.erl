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

-include_lib("wx/include/wx.hrl").
-include("ee_document.hrl").

-record(state, {win, buffer = [], caret_pos = 0}).
-record(main_window, {window, status_bar}).

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
	{ok, #state{win = MainWindow}}.

terminate(_Reason, _State) ->
	io:format("~p Cleaning up~n", [self()]),
	wx:destroy(),
	ok.

handle_call({get_buffer}, _From, #state{buffer = Buffer, caret_pos = CaretPos} = State) ->
	{reply, {Buffer, CaretPos}, State};
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
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_LEFT}}, #state{win = #main_window{window = Window}, caret_pos = CaretPos} = State) ->
	wxFrame:refresh(Window),
	{noreply, State#state{caret_pos = max(0, CaretPos - 1)}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_RIGHT}}, #state{win = #main_window{window = Window}, caret_pos = CaretPos} = State) ->
	wxFrame:refresh(Window),
	%% TODO: Limit caret pos to total buffer size.
	{noreply, State#state{caret_pos = CaretPos + 1}};
handle_info(#wx{event = #wxKey{type = char, uniChar = Char}}, #state{caret_pos = CaretPos} = State) ->
	%% Send message to data buffer to update.
	gen_server:cast(data_buffer, {char, [Char], CaretPos}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State#state{caret_pos = CaretPos + 1}};
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
	{Buffer, CaretPos} = gen_server:call(gui, {get_buffer}),
	draw_buffer(Window, Buffer, CaretPos),
	ok.

draw_buffer(Window, Buffer, CaretPos) ->
	DC = wxPaintDC:new(Window),
	ok = wxDC:clear(DC),
	ok = draw_buffer_lines(DC, Buffer),
	ok = draw_caret(DC, Buffer, CaretPos),
	wxPaintDC:destroy(DC),
	ok.

draw_buffer_lines(_DC, []) ->
	ok;
draw_buffer_lines(DC, [#buffer_line{num = Number, data = Data}|T]) ->
	ok = wxDC:drawText(DC, Data, {0, Number * 20}),
	draw_buffer_lines(DC, T).

draw_caret(_DC, [], _CaretPos) ->
	ok;
draw_caret(DC, [#buffer_line{num = Number, data = Data}|_], CaretPos) when CaretPos < length(Data) ->
	{W, H} = wxDC:getTextExtent(DC, lists:sublist(Data, CaretPos)),
	wxDC:drawLine(DC, {W, Number * 20}, {W, Number * 20 + H}),
	ok;
draw_caret(DC, [#buffer_line{data = Data}|T], CaretPos) ->
	draw_caret(DC, T, CaretPos - length(Data)).