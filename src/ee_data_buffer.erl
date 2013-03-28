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
	gen_server:start_link({local, data_buffer}, ?MODULE, [Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
	{filename, FileName} = proplists:lookup(filename, Args),
	{ok, File} = file:open(FileName, [read]),
	{ok, Buffer = [_|_]} = file:read(File, 100000),
	ok = file:close(File),
	{ok, #state{buffer = Buffer}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(display, State = #state{buffer = Buffer}) ->
	io:format("Displaying contents of file:~n~p~n", [Buffer]),
	{noreply, State};
handle_cast({subscribe, Pid}, State = #state{buffer = Buffer, subscribers = Subscribers}) ->
	gen_server:cast(Pid, {buffer, Buffer}),
	{noreply, State#state{subscribers = [Pid|Subscribers]}};
handle_cast({char, Char}, State = #state{buffer = Buffer, subscribers = [S]}) ->
	NewBuffer = Char ++ Buffer,
	gen_server:cast(S, {buffer, NewBuffer}),
	{noreply, State#state{buffer = NewBuffer}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
