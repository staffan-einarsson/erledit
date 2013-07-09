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

-record(ee_pubsub, {subscribers = []}).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
	#ee_pubsub{}.
	
add_subscriber(#ee_pubsub{subscribers = Subscribers}, Pid) ->
	#ee_pubsub{subscribers = [Pid|Subscribers]}.

remove_subscriber(#ee_pubsub{subscribers = Subscribers}, Pid) ->
	#ee_pubsub{subscribers = lists:delete(Pid, Subscribers)}.

publish(#ee_pubsub{subscribers = Subscribers}, Message) ->
	#ee_pubsub{subscribers = publish(Message, Subscribers, [])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

publish(_, [], ResList) ->
	lists:reverse(ResList);
publish(Message, [Subscriber|T], ResList) ->
	case catch Subscriber ! {?MODULE, Message, self()} of
		{'EXIT', {noproc, _}} ->
			publish(Message, T, ResList);
		_ ->
			publish(Message, T, [Subscriber|ResList])
	end.
	