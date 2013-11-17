%% --------------------------------------------------------------------
%% @author Staffan <staffan.einarsson@gmail.com>
%% @copyright 2013 Staffan Einarsson
%% @doc
%% @end
%% --------------------------------------------------------------------
%% Copyright 2013 Staffan Einarsson
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% --------------------------------------------------------------------

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