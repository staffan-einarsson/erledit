%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

%% Debug macro for printing an intermediate value.
-ifdef(debug).
-define(dbg_print(Term), erlang:apply(fun() -> Val = Term, io:format("dbg_print: ~p, ~p: ~p~n", [?FILE, ?LINE, Val]), Val end, [])).
-else.
-define(dbg_print(Term), Term).
-endif.
