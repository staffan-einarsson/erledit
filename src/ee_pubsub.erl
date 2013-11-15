%%% -------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc
%%% @end
%%% -------------------------------------------------------------------
%%% Copyright 2013 Staffan Einarsson
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% -------------------------------------------------------------------

-module(ee_pubsub).

%% API
-export([
	new/0,
	add_subscriber/2,
	remove_subscriber/2,
	publish/2
	]).

-include("ee_pubsub.hrl").

%%% -------------------------------------------------------------------
%%% API
%%% -------------------------------------------------------------------

new() ->
	#ee_pubsub_state{}.
	
add_subscriber(#ee_pubsub_state{subscribers = Subscribers}, Pid) ->
	#ee_pubsub_state{subscribers = [Pid|Subscribers]}.

remove_subscriber(#ee_pubsub_state{subscribers = Subscribers}, Pid) ->
	#ee_pubsub_state{subscribers = lists:delete(Pid, Subscribers)}.

publish(#ee_pubsub_state{subscribers = Subscribers}, Message) ->
	#ee_pubsub_state{subscribers = publish(Message, Subscribers, [])}.

%%% -------------------------------------------------------------------
%%% Internal functions
%%% -------------------------------------------------------------------

publish(_, [], ResList) ->
	lists:reverse(ResList);
publish(Message, [Subscriber|T], ResList) ->
	PubSubMessage = #ee_pubsub_message{message = Message, publisher = self()},
	case catch Subscriber ! PubSubMessage of
		{'EXIT', {noproc, _}} ->
			publish(Message, T, ResList);
		_ ->
			publish(Message, T, [Subscriber|ResList])
	end.
	