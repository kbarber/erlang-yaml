%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc Primary entry point for tests.
%%
%% @end
%% --------------------------

-module(yaml_tests).
-export([start/0]).

start() ->
    error_logger:tty(false),
    eunit:test(yaml_encoder,[verbose]),
    halt().
