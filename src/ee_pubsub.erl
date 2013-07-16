%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(ee_pubsub).

%% API
-export([
	new/0,
	add_subscriber/2,
	remove_subscriber/2,
	publish/2
	]).

-include("ee_pubsub.hrl").

%%%===================================================================
%%% API
%%%===================================================================

new() ->
	#ee_pubsub_state{}.
	
add_subscriber(#ee_pubsub_state{subscribers = Subscribers}, Pid) ->
	#ee_pubsub_state{subscribers = [Pid|Subscribers]}.

remove_subscriber(#ee_pubsub_state{subscribers = Subscribers}, Pid) ->
	#ee_pubsub_state{subscribers = lists:delete(Pid, Subscribers)}.

publish(#ee_pubsub_state{subscribers = Subscribers}, Message) ->
	#ee_pubsub_state{subscribers = publish(Message, Subscribers, [])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
	