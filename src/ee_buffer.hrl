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