%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc Primary entry point for tests.
%%
%% @end
%% --------------------------

-module(yaml_encoder_tests).
-include_lib("eunit/include/eunit.hrl").


%% @doc Feed in a single string and compare results with expectations
%% @end
basic_data_test() ->
    Expected = <<"---
teststring
">>,
    Returned = yaml_encoder:encode(<<"teststring">>),

    error_logger:info_msg("Comparing this: ~p~nwith this: ~p~n", 
        [ Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Feed in a basic sequence and check output matches what you expect.
%% @end
basic_sequence_test() ->
    Expected = <<"---
- foo
- bar
- baz
">>,
    Returned = yaml_encoder:encode([<<"foo">>,<<"bar">>,<<"baz">>]),

    error_logger:info_msg("Comparing this: ~p~nwith this: ~p~n", 
        [ Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Feed in a basic map and check output matches what you expect.
%% @end
basic_map_test() ->
    Expected = <<"---
foo: bar
baz: blah
asdf: fdsa
">>,
    Returned = yaml_encoder:encode([
        {<<"foo">>,<<"bar">>},
        {<<"baz">>,<<"blah">>},
        {<<"asdf">>,<<"fdsa">>}
    ]),

    error_logger:info_msg("Comparing this: ~p~nwith this: ~p~n", 
        [ Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).

