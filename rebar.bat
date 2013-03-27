@echo off

set cmd=%*
rem if "%cmd%"=="" (
rem 	set cmd=compile
rem )

escript rebar %cmd%
