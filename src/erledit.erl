%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(erledit).

%% API
-export([start/0, stop/0]).

-include("ee_global.hrl").

%% ===================================================================
%% API
%% ===================================================================

start() ->
	application:start(erledit).

stop() ->
	application:stop(erledit).