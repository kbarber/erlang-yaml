%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc yaml_encoder test routines
%%
%% Tests here must always aim for 1.2 compliance of the YAML specification: 
%%
%% http://www.yaml.org/spec/1.2/spec.html
%%
%% @end
%% --------------------------

-module(yaml_encoder_tests).
-include_lib("eunit/include/eunit.hrl").


%% @doc Feed in a single string and compare results with expectations
%% @end
basic_data_test() ->
    Test = "basic_data",
    Expected = <<"---
teststring
">>,
    Returned = yaml_encoder:encode(<<"teststring">>),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n", 
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Feed in a basic sequence and check output matches what you expect.
%% @end
basic_sequence_test() ->
    Test = "basic_sequence",
    Expected = <<"---
- foo
- bar
- baz
">>,
    Returned = yaml_encoder:encode([<<"foo">>,<<"bar">>,<<"baz">>]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n", 
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Feed in a basic map and check output matches what you expect.
%% @end
basic_map_test() ->
    Test = "basic_map",
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

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n", 
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Feed in a basic map and check output matches what you expect.
%% @end
map_of_sequences_test() ->
    Test = "map_of_sequences",
    Expected = <<"---
foo:
  - foo
  - bar
baz:
  - asdf
  - fdsa
asdf:
  - aaaa
  - bbbb
">>,
    Returned = yaml_encoder:encode([
        {<<"foo">>,[<<"foo">>,<<"bar">>]},
        {<<"baz">>,[<<"asdf">>,<<"fdsa">>]},
        {<<"asdf">>,[<<"aaaa">>,<<"bbbb">>]}
    ]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n",
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Feed in a basic map and check output matches what you expect.
%% @end
map_of_empty_sequences_test() ->
    Test = "map_of_empty_sequences",
    Expected = <<"---
foo: []
baz: []
asdf: []
">>,
    Returned = yaml_encoder:encode([
        {<<"foo">>,[]},
        {<<"baz">>,[]},
        {<<"asdf">>,[]}
    ]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n",
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Sequence of maps
%% @end
sequence_of_maps_test() ->
    Test = "sequence_of_maps",
    Expected = <<"---
-
  a: b
  b: c
-
  d: 3
  e: f
">>,
    Returned = yaml_encoder:encode([
        [
            {<<"a">>,<<"b">>},
            {<<"b">>,<<"c">>}
        ],
        [
            {<<"d">>,3},
            {<<"e">>,<<"f">>}
        ]
    ]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n",
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Sequence of Sequences test
%% @end
sequence_of_sequences_test() ->
    Test = "sequence_of_sequences",
    Expected = <<"---
-
  - a
  - b
-
  - a
  - b
">>,
    Returned = yaml_encoder:encode([
        [<<"a">>,<<"b">>],
        [<<"a">>,<<"b">>]
    ]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n",
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Map of map test
%% @end
map_of_map_test() ->
    Test = "map_of_map",
    Expected = <<"---
foo:
  foo: a
  bar: b
baz:
  asdf: a
  fdsa: b
asdf:
  aaaa: a
  bbbb: b
">>,
    Returned = yaml_encoder:encode([
        {<<"foo">>,[
            {<<"foo">>,<<"a">>},
            {<<"bar">>,<<"b">>}
        ]},
        {<<"baz">>,[
            {<<"asdf">>,<<"a">>},
            {<<"fdsa">>,<<"b">>}
        ]},
        {<<"asdf">>,[
            {<<"aaaa">>,<<"a">>},
            {<<"bbbb">>,<<"b">>}
        ]}
    ]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n",
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).


%% @doc Very complex
%% @end
very_complex_test() ->
    Test = "very_complex",
    Expected = <<"---
foo:
  foo:
    - test
    -
      foo: 123
      bar: asdf
  baz:
    a: b
    b: c
    d:
      -
        - a
        -
          a: b
      - asdf
baz:
  asdf: a
  fdsa: b
asdf:
  aaaa: a
  bbbb: b
">>,
    Returned = yaml_encoder:encode([
        {<<"foo">>,[
            {<<"foo">>,[
                <<"test">>,
                [
                    {<<"foo">>,123},
                    {<<"bar">>, <<"asdf">>}
                ]
            ]},
            {<<"baz">>,[
                {<<"a">>,<<"b">>},
                {<<"b">>,<<"c">>},
                {<<"d">>,[
                    [
                        <<"a">>,
                        [
                            {<<"a">>,<<"b">>}
                        ]
                    ],
                    <<"asdf">>
                ]}
            ]}
        ]},
        {<<"baz">>,[
            {<<"asdf">>,<<"a">>},
            {<<"fdsa">>,<<"b">>}
        ]},
        {<<"asdf">>,[
            {<<"aaaa">>,<<"a">>},
            {<<"bbbb">>,<<"b">>}
        ]}
    ]),

    ?debugFmt("~nTest: ~p~nExpect: ~p~nReturn: ~p~n",
        [ Test, Expected, Returned ]
    ),

    ?assert(string:equal(Expected, Returned)).
