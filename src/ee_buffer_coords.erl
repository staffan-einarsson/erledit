%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_buffer_coords).

%% API
-export(
		[
		new/2
		]
	).

-include_lib("eunit/include/eunit.hrl").
-include("ee_buffer_coords.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------

new(
		LineNo,
		_
	)
		when LineNo < 1
	->
		erlang:error(bad_line_no);
new(
		_,
		LineOffset
	)
		when LineOffset < 1
	->
		erlang:error(bad_line_offset);
new(
		LineNo,
		LineOffset
	)
	->
		#ee_buffer_coords{line_no = LineNo, line_offset = LineOffset}.

new_test_()
	->
		[
		?_assertEqual(#ee_buffer_coords{line_no = 99, line_offset = 66}, new(99, 66)),
		?_assertError(bad_line_no, new(0, 1)),
		?_assertError(bad_line_no, new(-99, 1)),
		?_assertError(bad_line_offset, new(1, 0)),
		?_assertError(bad_line_offset, new(1, -66))
		].
