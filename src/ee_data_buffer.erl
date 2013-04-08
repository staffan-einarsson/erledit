%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_data_buffer).
-behaviour(gen_server).

%% API
-export([
	start_link/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("ee_document.hrl").

-record(state, {buffer=[], subscribers=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid::pid()}
%% @end
%%--------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, data_buffer}, ?MODULE, [Args], [
		%{debug, [trace]}
		]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
	{filename, FileName} = proplists:lookup(filename, Args),
	{ok, File} = file:open(FileName, [read]),
	{ok, Buffer = [_|_]} = file:read(File, 100000),
	ok = file:close(File),
	LineStructs = split_buffer(Buffer),
	%io:format("~p~n", [LineStructs]),
	{ok, #state{buffer = LineStructs}}.

terminate(_Reason, _State) ->
	ok.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(display, #state{buffer = Buffer} = State) ->
	io:format("Displaying contents of file:~n~p~n", [Buffer]),
	{noreply, State};
handle_cast({get_buffer, Pid}, #state{buffer = Buffer} = State) ->
	gen_server:cast(Pid, {buffer, Buffer}),
	{noreply, State};
handle_cast({char, Char, Caret}, #state{buffer = Buffer} = State) ->
	{noreply, State#state{buffer = insert_text(Buffer, Char, Caret)}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert_text([#buffer_line{num = LineNo, data = Data} = Line|T], Text, #caret{line = LineNo, column = Column}) ->
	{A, B} = lists:split(Column, Data),
	[Line#buffer_line{data = A ++ (Text ++ B)}|T];
insert_text([Line|T], Text, Caret) ->
	%% TODO: Tail recursion.
	[Line|insert_text(T, Text, Caret)].

split_buffer(Buffer) ->
	split_buffer_loop(Buffer, 0, [], []).

split_buffer_loop([], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	lists:reverse([#buffer_line{num = CurrentLineNo, data = lists:reverse(CurrentLineBufferRev)}|PrevLinesRev]);
split_buffer_loop([?ASCII_LF|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo + 1, [], [#buffer_line{num = CurrentLineNo, data = lists:reverse([?ASCII_LF|CurrentLineBufferRev])}|PrevLinesRev]);
split_buffer_loop([?ASCII_CR, ?ASCII_LF|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo + 1, [], [#buffer_line{num = CurrentLineNo, data = lists:reverse([?ASCII_LF, ?ASCII_CR|CurrentLineBufferRev])}|PrevLinesRev]);
split_buffer_loop([Char|T], CurrentLineNo, CurrentLineBufferRev, PrevLinesRev) ->
	split_buffer_loop(T, CurrentLineNo, [Char|CurrentLineBufferRev], PrevLinesRev).
