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

-module(ee_document_controller).
-behaviour(gen_server).

%% API
-export([
	start_link/0,
	insert/1,
	delete/1,
	get_documents/0,
	add_subscriber/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("ee_global.hrl").

-record(state, {documents = [], pubsub_state}).

%%% -------------------------------------------------------------------
%%% API
%%% -------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Pid) ->
	gen_server:call(?MODULE, {insert, Pid}).

delete(Pid) ->
	gen_server:call(?MODULE, {delete, Pid}).

get_documents() ->
	gen_server:call(?MODULE, {get_documents}).
	
add_subscriber(Pid) ->
	gen_server:cast(?MODULE, {add_subscriber, Pid}).

%%% -------------------------------------------------------------------
%%% gen_server callbacks
%%% -------------------------------------------------------------------

init([]) ->
	{ok, #state{documents = [], pubsub_state = ee_pubsub:new()}}.

terminate(_Reason, _State) ->
	ok.

handle_call({insert, Pid}, _From, #state{documents = Documents0, pubsub_state = PubSubState0} = State) ->
	Documents1 = lists:append([Pid], Documents0),
	PubSubState1 = ee_pubsub:publish(PubSubState0, {document_inserted, Pid}),
	{reply, {ok, Pid}, State#state{documents = Documents1, pubsub_state = PubSubState1}};
handle_call({delete, Pid}, _From, #state{documents = Documents0, pubsub_state = PubSubState0} = State) ->
	Documents1 = lists:delete(Pid, Documents0),
	PubSubState1 = ee_pubsub:publish(PubSubState0, {document_deleted, Pid}),
	{reply, {ok, Pid}, State#state{documents = Documents1, pubsub_state = PubSubState1}};
handle_call({get_documents}, _From, #state{documents = Documents} = State) ->
	{reply, {ok, Documents}, State};
handle_call(Msg, _From, _State) ->
	erlang:error({bad_call, Msg}).

handle_cast({add_subscriber, Pid}, #state{pubsub_state = PubSubState0} = State) ->
	PubSubState1 = ee_pubsub:add_subscriber(PubSubState0, Pid),
	{noreply, State#state{pubsub_state = PubSubState1}};
handle_cast(Msg, _State) ->
	erlang:error({bad_cast, Msg}).

handle_info(Msg, _State) ->
	erlang:error({bad_info, Msg}).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% -------------------------------------------------------------------
%%% Internal functions
%%% -------------------------------------------------------------------
