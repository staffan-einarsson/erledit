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

-record(state, {buffer}).

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
	{ok, String = [_|_]} = file:read(File, 100000),
	ok = file:close(File),
	{ok, #state{buffer = ee_buffer:create_from_string(String)}}.

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
handle_cast({char, Char, #caret{line = LineNo, column = ColNo}}, #state{buffer = Buffer} = State) ->
	{noreply, State#state{buffer = ee_buffer:insert_text(Buffer, Char, LineNo, ColNo)}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
