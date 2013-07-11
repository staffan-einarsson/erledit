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
	start_link/1,
	add_subscriber/2,
	display/1,
	get_buffer/1,
	remove_left/2,
	remove_right/2,
	insert_eol/2,
	insert_text/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("ee_global.hrl").
-include("ee_document.hrl").
-include("ee_buffer.hrl").

-record(state, {buffer, pubsub_state}).

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


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
	{filename, FileName} = proplists:lookup(filename, Args),
	{ok, #state{buffer = load_buffer_from_file(FileName), pubsub_state = ee_pubsub:new()}, 0}.

terminate(_Reason, _State) ->
	ee_document_controller:delete(self()),
	ok.

handle_call({get_buffer}, _From, #state{buffer = Buffer} = State) ->
	{reply, {ok, Buffer}, State};
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

publish_buffer(PubsubState, Buffer) ->
	ee_pubsub:publish(PubsubState, {buffer_update, Buffer}).
