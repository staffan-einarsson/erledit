%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_document_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("ee_global.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{one_for_one, 5, 10}, [?CHILD(ee_buffer_server, worker, [{filename, "readme"}]), ?CHILD(ee_gui, worker, [])]}}.

