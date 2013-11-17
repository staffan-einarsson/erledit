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

-module(ee_document_sup).
-behaviour(supervisor).

%% API
-export([
	start_link/0,
	open_document/1,
	open_document/0
	]).

%% Supervisor callbacks
-export([init/1]).

-include("ee_global.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, transient, 5000, Type, [I]}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

open_document(FileName) ->
	{ok, _} = supervisor:start_child(?MODULE, [[{filename, FileName}]]).

open_document() ->
	{ok, _} = supervisor:start_child(?MODULE, [[]]).

%% --------------------------------------------------------------------
%% Supervisor callbacks
%% --------------------------------------------------------------------

init([]) ->
	{ok, {{simple_one_for_one, 5, 10}, [?CHILD(ee_document, worker, [])]}}.

