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

%-compile(export_all).

-include_lib("wx/include/wx.hrl").

-record(state, {win, buffer=[]}).
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
	gen_server:cast(data_buffer, {subscribe, self()}),
	{ok, #state{win = MainWindow}}.

terminate(_Reason, _State) ->
	io:format("~p Cleaning up~n", [self()]),
	wx:destroy(),
	ok.

handle_call(Msg, _From, State) ->
	io:format("~p Got Call ~p~n", [self(), Msg]),
	{reply, ok, State}.

handle_cast({buffer, Buffer}, #state{win = #main_window{window = Window}} = State) ->
	%% TODO: Update text by triggering paint events instead, setting updated areas as dirty.
	DC = wxClientDC:new(Window),
	ok = wxDC:clear(DC),
	ok = wxDC:drawText(DC, Buffer, {0, 0}),
	wxClientDC:destroy(DC),
	{noreply, State#state{buffer = Buffer}};
handle_cast(Msg, State) ->
	io:format("~p Got Cast ~p~n", [self(), Msg]),
	{noreply, State}.

handle_info(#wx{event = #wxClose{}}, #state{win = #main_window{window = Window}} = State) ->
	io:format("~p Closing window ~n", [self()]),
	ok = wxFrame:setStatusText(Window, "Closing...",[]),
	wxWindow:destroy(Window),
	{stop, normal, State};
handle_info(#wx{event = #wxKey{type = char, uniChar = Char}}, State) ->
	%% Send message to data buffer to update.
	gen_server:cast(data_buffer, {char, [Char]}),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("~p Got Info ~p~n", [self(), Msg]),
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

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

	%% Show window.
	wxWindow:show(Window),
	#main_window{window = Window, status_bar = StatusBar}.
