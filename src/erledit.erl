-module(erledit).

%% API
-export([start/0, stop/0]).

%% ===================================================================
%% API
%% ===================================================================

start() ->
	application:start(erledit).

stop() ->
	application:stop(erledit).