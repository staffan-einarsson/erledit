%%% -------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc
%%% @end
%%% -------------------------------------------------------------------
%%% Copyright 2013 Staffan Einarsson
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% -------------------------------------------------------------------

-module(ee_document).
-behaviour(gen_server).

%% API
-export([
	start_link/1,
	add_subscriber/2,
	display/1,
	get_buffer/1,
	remove_left/2,
	remove_right/2,
	insert_eol/2,
	insert_text/3,
	save_file/1,
	set_filename/2,
	close/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("ee_global.hrl").
-include("ee_buffer_coords.hrl").

-record(state, {filename, buffer, pubsub_state}).

%%% -------------------------------------------------------------------
%%% API
%%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%% --------------------------------------------------------------------
-spec start_link(list()) -> ok.
start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], [
		%{debug, [trace]}
		]).

add_subscriber(BufferServerPid, SubscriberPid) ->
	gen_server:cast(BufferServerPid, {add_subscriber, SubscriberPid}).

display(BufferServerPid) ->
	gen_server:cast(BufferServerPid, {display}).

get_buffer(BufferServerPid) ->
	gen_server:call(BufferServerPid, {get_buffer}).

remove_left(BufferServerPid, #ee_buffer_coords{} = Coords) ->
	gen_server:cast(BufferServerPid, {remove_left, Coords}).

remove_right(BufferServerPid, #ee_buffer_coords{} = Coords) ->
	gen_server:cast(BufferServerPid, {remove_right, Coords}).

insert_eol(BufferServerPid, #ee_buffer_coords{} = Coords) ->
	gen_server:cast(BufferServerPid, {insert_eol, Coords}).

insert_text(BufferServerPid, Text, #ee_buffer_coords{} = Coords) ->
	gen_server:cast(BufferServerPid, {insert_text, Text, Coords}).

save_file(BufferServerPid) ->
	gen_server:call(BufferServerPid, {try_save_file}).

set_filename(BufferServerPid, FileName) ->
	gen_server:cast(BufferServerPid, {set_filename, FileName}).

close(BufferServerPid) ->
	gen_server:cast(BufferServerPid, {close}).

%%% -------------------------------------------------------------------
%%% gen_server callbacks
%%% -------------------------------------------------------------------

init([Args]) ->
	FileName = case proplists:lookup(filename, Args) of
		{filename, FN} -> FN;
		none -> undefined
	end,
	{ok, #state{filename = FileName, pubsub_state = ee_pubsub:new()}, 0}.

terminate(_Reason, _State) ->
	ee_document_controller:delete(self()),
	ok.

handle_call({get_buffer}, _From, #state{buffer = Buffer} = State) ->
	{reply, {ok, Buffer}, State};
handle_call({try_save_file}, _From, #state{filename = FileName} = State) ->
	Result = check_filename(FileName),
	case Result of
		ok -> gen_server:cast(self(), {save_file});
		_ -> false
	end,
	{reply, Result, State};
handle_call(Msg, _From, _State) ->
	erlang:error({bad_call, Msg}).

handle_cast({add_subscriber, Pid}, #state{pubsub_state = PubSubState0} = State) ->
	PubSubState1 = ee_pubsub:add_subscriber(PubSubState0, Pid),
	{noreply, State#state{pubsub_state = PubSubState1}};
handle_cast({display}, #state{buffer = Buffer} = State) ->
	io:format("Displaying contents of file:~n~p~n", [Buffer]),
	{noreply, State};
handle_cast({remove_left, Coords}, #state{buffer = Buffer0, pubsub_state = PubsubState0} = State) ->
	Buffer1 = ee_buffer:remove_left(Buffer0, Coords),
	PubsubState1 = publish_buffer(PubsubState0, Buffer1),
	{noreply, State#state{buffer = Buffer1, pubsub_state = PubsubState1}};
handle_cast({remove_right, Coords}, #state{buffer = Buffer0, pubsub_state = PubsubState0} = State) ->
	Buffer1 = ee_buffer:remove_right(Buffer0, Coords),
	PubsubState1 = publish_buffer(PubsubState0, Buffer1),
	{noreply, State#state{buffer = Buffer1, pubsub_state = PubsubState1}};
handle_cast({insert_eol, #ee_buffer_coords{} = Coords}, #state{buffer = Buffer0, pubsub_state = PubsubState0} = State) ->
	Buffer1 = ee_buffer:insert_eol(Buffer0, Coords),
	PubsubState1 = publish_buffer(PubsubState0, Buffer1),
	{noreply, State#state{buffer = Buffer1, pubsub_state = PubsubState1}};
handle_cast({insert_text, Text, #ee_buffer_coords{} = Coords}, #state{buffer = Buffer0, pubsub_state = PubsubState0} = State) ->
	Buffer1 = ee_buffer:insert_text(Buffer0, Text, Coords),
	PubsubState1 = publish_buffer(PubsubState0, Buffer1),
	{noreply, State#state{buffer = Buffer1, pubsub_state = PubsubState1}};
handle_cast({save_file}, #state{filename = FileName, buffer = Buffer} = State) ->
	save_buffer_to_file(Buffer, FileName),
	{noreply, State};
handle_cast({set_filename, FileName}, State0) ->
	State1 = State0#state{filename = FileName},
	{noreply, State1};
handle_cast({close}, State) ->
	{stop, normal, State};
handle_cast(Msg, _State) ->
	erlang:error({bad_cast, Msg}).

handle_info(timeout, #state{filename = FileName} = State0) ->
	ee_document_controller:insert(self()),
	Buffer = case FileName of
		undefined -> ee_buffer:new();
		_ -> load_buffer_from_file(FileName)
	end,
	State1 = State0#state{buffer = Buffer},
	{noreply, State1};
handle_info(Msg, _State) ->
	erlang:error({bad_info, Msg}).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% -------------------------------------------------------------------
%%% Internal functions
%%% -------------------------------------------------------------------

load_buffer_from_file(FilePath) ->
	{ok, File} = file:open(FilePath, [read]),
	{ok, String = [_|_]} = file:read(File, 100000),
	ok = file:close(File),
	ee_buffer:create_from_string(String).

save_buffer_to_file(Buffer, FilePath) ->
	String = ee_buffer:to_string(Buffer),
	{ok, File} = file:open(FilePath, [write]),
	ok = file:write(File, String),
	ok = file:close(File).

publish_buffer(PubsubState, Buffer) ->
	ee_pubsub:publish(PubsubState, {buffer_update, Buffer}).

check_filename(undefined) ->
	no_filename;
check_filename(_) ->
	ok.
