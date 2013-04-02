rem @echo off

call rebar clean
call rebar compile

erl -pa ebin -run erledit
