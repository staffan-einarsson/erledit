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

-module(erledit_test).

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

start_and_stop_should_not_throw_test()
	->
		erledit:start(),
		erledit:stop().
