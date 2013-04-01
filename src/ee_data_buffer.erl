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
	Lines = string:tokens(Buffer, "\n"),
	{LineStructs, _} = lists:mapfoldl(
		fun(LineData, LineNo) ->
			{#buffer_line{num = LineNo, data = LineData}, LineNo + 1}
			end,
		0, Lines),
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
handle_cast({char, Char}, #state{buffer = [#buffer_line{data = Buffer0} = Line|T]} = State) ->
	Buffer1 = Char ++ Buffer0,
	{noreply, State#state{buffer = [Line#buffer_line{data = Buffer1}|T]}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
