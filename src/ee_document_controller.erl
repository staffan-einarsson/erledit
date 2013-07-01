%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_document_controller).

%% API
-export([
	init/0,
	insert/1,
	delete/1
	]).

-include("ee_global.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init() ->
	ets:new(?MODULE, [public, named_table]).

insert(Pid) ->
	ets:insert(?MODULE, {Pid, Pid}).

delete(Pid) ->
	ets:delete(?MODULE, Pid).

%%%===================================================================
%%% Internal functions
%%%===================================================================
