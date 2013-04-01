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

handle_call({get_buffer}, _From, #state{buffer = Buffer} = State) ->
	{reply, Buffer, State};
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
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_LEFT}}, #state{caret_pos = CaretPos} = State) ->
	{noreply, State#state{caret_pos = CaretPos - 1}};
handle_info(#wx{event = #wxKey{type = char, keyCode = ?WXK_RIGHT}}, #state{caret_pos = CaretPos} = State) ->
	{noreply, State#state{caret_pos = CaretPos + 1}};
handle_info(#wx{event = #wxKey{type = char, uniChar = Char}}, State) ->
	%% Send message to data buffer to update.
	gen_server:cast(data_buffer, {char, [Char]}),
	gen_server:cast(data_buffer, {get_buffer, self()}),
	{noreply, State};
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
	Buffer = gen_server:call(gui, {get_buffer}),
	draw_buffer(Window, Buffer),
	ok.

draw_buffer(Window, Buffer) ->
	DC = wxPaintDC:new(Window),
	ok = wxDC:clear(DC),
	ok = draw_buffer_lines(DC, Buffer),
	wxPaintDC:destroy(DC),
	ok.

draw_buffer_lines(_DC, []) ->
	ok;
draw_buffer_lines(DC, [#buffer_line{num = Number, data = Data}|T]) ->
	ok = wxDC:drawText(DC, Data, {0, Number * 20}),
	draw_buffer_lines(DC, T).