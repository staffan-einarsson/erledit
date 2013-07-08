%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

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

-record(state, {documents = [], subscribers = []}).

%%%===================================================================
%%% API
%%%===================================================================

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

handle_call({insert, Pid}, _From, #state{documents = Documents, subscribers = Subscribers} = State) ->
	lists:foreach(fun(Subscriber) -> gen_server:cast(Subscriber, {document_inserted, Pid}) end, Subscribers),
	{reply, {ok, Pid}, State#state{documents = lists:append([Pid], Documents)}};
handle_call({delete, Pid}, _From, #state{documents = Documents} = State) ->
	{reply, {ok, Pid}, State#state{documents = lists:delete(Pid, Documents)}};
handle_call({get_documents}, _From, #state{documents = Documents} = State) ->
	{reply, {ok, Documents}, State};
handle_call(_Request, _From, State) ->
	{stop, bad_call, State}.

handle_cast({add_subscriber, Pid}, #state{subscribers = Subscribers} = State) ->
	{noreply, State#state{subscribers = [Pid|Subscribers]}};
handle_cast(_Msg, State) ->
	{stop, bad_cast, State}.

handle_info(_Info, State) ->
	{stop, bad_info, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
