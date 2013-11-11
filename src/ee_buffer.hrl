%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-type eol_token() :: eol_lf | eol_crlf | none.
-type ee_buffer_line() :: {ee_buffer_line, integer(), string(), eol_token()}.
-record(ee_buffer_line, {
	line_no = 1 :: integer(),
	contents = "" :: string(),
	eol = none :: eol_token()
	}).

-type ee_buffer() :: {ee_buffer, [ee_buffer_line()]}.
-record(ee_buffer, {
	lines = [] :: [ee_buffer_line()]
	}).

-define(ASCII_TAB, 9).
-define(ASCII_LF, 10).
-define(ASCII_CR, 13).
