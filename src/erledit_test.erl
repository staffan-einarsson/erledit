%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(erledit_test).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================

start_and_stop_should_not_throw_test() ->
	erledit:start(),
	erledit:stop().
