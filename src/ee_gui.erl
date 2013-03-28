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

-record(state, {win}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	wx:new(),
	Frame = create_window(),
	{ok, #state{win=Frame}}.

terminate(_Reason, _State) ->
	io:format("~p Cleaning up~n", [self()]),
	wx:destroy(),
	ok.

handle_call(Msg, _From, State) ->
	io:format("~p Got Call ~p~n", [self(), Msg]),
	{reply, ok, State}.

handle_cast(Msg, State) ->
	io:format("~p Got Cast ~p~n", [self(), Msg]),
	{noreply, State}.

handle_info(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
	io:format("~p Closing window ~n", [self()]),
	ok = wxFrame:setStatusText(Frame, "Closing...",[]),
	wxWindow:destroy(Frame),
	{stop, normal, State};
handle_info(Msg, State) ->
	io:format("~p Got Info ~p~n", [self(), Msg]),
	{noreply,State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

create_window() ->
	Frame = wxFrame:new(wx:null(), 
			-1, % window id
			"Hello World", % window title
			[{size, {600,400}}]),

	wxFrame:createStatusBar(Frame),
	ok = wxFrame:setStatusText(Frame, "Hello World!",[]),

	%% Subscribe to events.
	wxFrame:connect(Frame, close_window),

	wxWindow:show(Frame),
	Frame.
