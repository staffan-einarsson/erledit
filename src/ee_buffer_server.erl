%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_buffer_server).
-behaviour(gen_server).

%% API
-export([
	start_link/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("ee_global.hrl").
-include("ee_document.hrl").
-include("ee_buffer.hrl").

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
	gen_server:start_link(?MODULE, [Args], [
		%{debug, [trace]}
		]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
	{filename, FileName} = proplists:lookup(filename, Args),
	{ok, #state{buffer = load_buffer_from_file(FileName)}, 0}.

terminate(_Reason, _State) ->
	ee_document_controller:delete(self()),
	ok.

handle_call(Msg, _From, _State) ->
	erlang:error({bad_call, Msg}).

handle_cast(display, #state{buffer = Buffer} = State) ->
	io:format("Displaying contents of file:~n~p~n", [Buffer]),
	{noreply, State};
handle_cast({get_buffer, Pid}, #state{buffer = Buffer} = State) ->
	gen_server:cast(Pid, {buffer, Buffer}),
	{noreply, State};
handle_cast({delete_left, #ee_buffer_coords{} = Coords}, #state{buffer = Buffer} = State) ->
	{noreply, State#state{buffer = ee_buffer:remove_left(Buffer, Coords)}};
handle_cast({delete_right, #ee_buffer_coords{} = Coords}, #state{buffer = Buffer} = State) ->
	{noreply, State#state{buffer = ee_buffer:remove_right(Buffer, Coords)}};
handle_cast({eol, #ee_buffer_coords{} = Coords}, #state{buffer = Buffer} = State) ->
	{noreply, State#state{buffer = ee_buffer:insert_eol(Buffer, Coords)}};
handle_cast({char, Char, #ee_buffer_coords{} = Coords}, #state{buffer = Buffer} = State) ->
	{noreply, State#state{buffer = ee_buffer:insert_text(Buffer, Char, Coords)}};
handle_cast(Msg, _State) ->
	erlang:error({bad_cast, Msg}).

handle_info(timeout, State) ->
	ee_document_controller:insert(self()),
	{noreply, State};
handle_info(Msg, _State) ->
	erlang:error({bad_info, Msg}).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_buffer_from_file(FilePath) ->
	{ok, File} = file:open(FilePath, [read]),
	{ok, String = [_|_]} = file:read(File, 100000),
	ok = file:close(File),
	ee_buffer:create_from_string(String).