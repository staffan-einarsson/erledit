%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-record(ee_buffer_line, {num = 0, data = []}).
-record(ee_buffer, {lines = []}).

-define(ASCII_TAB, 9).
-define(ASCII_LF, 10).
-define(ASCII_CR, 13).
