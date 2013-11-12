%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(ee_buffer_line).

%% API
-export(
		[
		new/0,
		new/3,
		get_line_number/1,
		get_line_contents/1,
		get_line_length/1
		]
	).

-include_lib("eunit/include/eunit.hrl").
-include("ee_global.hrl").
-include("ee_buffer_line.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Creates a new empty ee_buffer_line object.
%% @end
%%--------------------------------------------------------------------
-spec new() -> ee_buffer_line().
new()
	->
		#ee_buffer_line{line_no = 1, contents = "", eol = none}.

%%--------------------------------------------------------------------
%% @doc Creates a new ee_buffer_line object from the given arguments.
%% @end
%%--------------------------------------------------------------------
-spec new(LineNo :: integer(), Contents :: string(), Eol :: eol_lf | eol_crlf | none) -> ee_buffer_line().
new(
		LineNo,
		Contents,
		Eol
	)
	->
		#ee_buffer_line{line_no = LineNo, contents = Contents, eol = Eol}.

%%--------------------------------------------------------------------

new_test_()
	->
		[
		?_assertEqual(
			#ee_buffer_line{line_no = 1, contents = "", eol = none},
			new()
			),
		?_assertEqual(
			#ee_buffer_line{line_no = 55, contents = "Hello", eol = eol_lf},
			new(55, "Hello", eol_lf)
			)
		].

%%--------------------------------------------------------------------
%% @doc Returns the line number of a ee_buffer_line object.
%% @end
%%--------------------------------------------------------------------
-spec get_line_number(Line :: ee_buffer_line()) -> integer().
get_line_number(
		#ee_buffer_line{line_no = LineNo}
	)
	->
		LineNo.

%%--------------------------------------------------------------------

get_line_number_test_()
	->
		[
		?_assertEqual(
			5,
			get_line_number(#ee_buffer_line{line_no = 5})
			)
		].

%%--------------------------------------------------------------------
%% @doc Returns the contents of a ee_buffer_line object.
%% @end
%%--------------------------------------------------------------------
-spec get_line_contents(Line :: ee_buffer_line()) -> string().
get_line_contents(
		#ee_buffer_line{contents = Contents}
	)
	->
		Contents.

%%--------------------------------------------------------------------

get_line_contents_test_()
	->
		[
		?_assertEqual(
			"Hello",
			get_line_contents(#ee_buffer_line{contents = "Hello"})
			)
		].

%%--------------------------------------------------------------------
%% @doc Returns the contents of a ee_buffer_line object.
%% @end
%%--------------------------------------------------------------------
-spec get_line_length(Line :: ee_buffer_line()) -> integer().
get_line_length(
		#ee_buffer_line{contents = Contents}
	)
	->
		length(Contents).

%%--------------------------------------------------------------------

get_line_length_test_()
	->
		[
		?_assertEqual(
			8,
			get_line_length(
				#ee_buffer_line{line_no = 2, contents = "TextText", eol = eol_lf}
				)
			)
		].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
