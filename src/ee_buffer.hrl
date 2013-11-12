%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-ifndef(EE_BUFFER_HRL).
-define(EE_BUFFER_HRL, 1).

-include("ee_buffer_line.hrl").

-type ee_buffer() :: {ee_buffer, [ee_buffer_line()]}.
-record(ee_buffer, {
	lines = [] :: [ee_buffer_line()]
	}).

-define(ASCII_TAB, 9).
-define(ASCII_LF, 10).
-define(ASCII_CR, 13).

-endif.