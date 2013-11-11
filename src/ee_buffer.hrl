%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

%% eol can be eol_lf, eol_crlf, or none
-type ee_buffer_line() :: tuple().
-record(ee_buffer_line, {line_no = 1, contents = [], eol = none}).

-type ee_buffer() :: tuple().
-record(ee_buffer, {lines = []}).

-define(ASCII_TAB, 9).
-define(ASCII_LF, 10).
-define(ASCII_CR, 13).
