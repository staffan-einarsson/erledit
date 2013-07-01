%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(erledit_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("ee_global.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ee_document_controller:init(),
	ee_sup:start_link(),
	ee_document_sup:start_child().

stop(_State) ->
	ok.
