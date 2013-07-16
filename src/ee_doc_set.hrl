%%%-------------------------------------------------------------------
%%% @author Staffan <staffan.einarsson@gmail.com>
%%% @copyright 2013 Staffan Einarsson
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-include("ee_caret.hrl").

-record(ee_doc_view, {pid, buffer, caret = #ee_caret{}}).
-record(ee_doc_set, {focus_doc, background_docs = []}).
