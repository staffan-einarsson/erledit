%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-ifndef(EE_BUFFER_LINE_HRL).
-define(EE_BUFFER_LINE_HRL, 1).

-type eol_token() :: eol_lf | eol_crlf | none.
-type ee_buffer_line() :: {ee_buffer_line, integer(), string(), eol_token()}.
-record(ee_buffer_line, {
	line_no = 1 :: integer(),
	contents = "" :: string(),
	eol = none :: eol_token()
	}).

-endif.