%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-record(ee_pubsub_state, {subscribers = []}).
-record(ee_pubsub_message, {message, publisher}).
