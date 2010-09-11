%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc YAML encoder
%%

%%
%% @end
%% --------------------------
-module(yaml_encoder).

-export([encode/1]).

%% @doc Encode erlang data structure into YAML text.
%%
%% Returns a YAML encoded document as a binary string. See examples.txt in docs/
%% for examples of the corresponding formats.
%%
%% @end 
encode(Data) ->
    % Run iteration functions return results (with header)
    Encoded = encode_data(Data,0),
    case Data of
        [_|_] ->
            <<"---\n", Encoded/binary>>;
        _ ->
            <<"--- ", Encoded/binary>>
    end.
    
%% @doc
%%
%% encode_data(Data, Level)
%% returns: CompleteDocument
%% @end
encode_data({Key,Val},Level) ->
    % Its a map, grab the key, and re-run encode_data on the value, using
    % the results in your return
    Indent = indent(Level),

    case Val of
        [_|_] -> 
            EncodedVal = encode_data(Val,Level+1),
            <<Indent/binary, Key/binary, ":\n", EncodedVal/binary>>;
        [] ->
            % Empty sequences will draw an empty sequence in flow format
            <<Indent/binary, Key/binary, ": []\n">>;
        _Scalar ->
            EncodedVal = encode_data(Val,Level),
            <<Indent/binary, Key/binary, ": ", EncodedVal/binary>>
    end;

encode_data([First|Remainder],Level) ->
    % Its a sequence, so I should iterate across it
    EncodedSequence = encode_data(Remainder,Level),
    Indent = indent(Level),

    case First of
        {_Key,_Val} -> 
            EncodedFirst = encode_data(First,Level),
            <<EncodedFirst/binary, EncodedSequence/binary>>;
        [{_,_}|_] ->
            EncodedFirst = encode_data(First,Level+1),
            <<Indent/binary, "-\n", EncodedFirst/binary, EncodedSequence/binary>>;
        [_|_] ->
            EncodedFirst = encode_data(First,Level+1),
            <<Indent/binary, "-\n", EncodedFirst/binary, EncodedSequence/binary>>;
        _Scalar -> 
            EncodedFirst = encode_data(First,Level+1),
            <<Indent/binary, "- ", EncodedFirst/binary, EncodedSequence/binary>>
    end;

encode_data([],_Level) ->
    % ran out of list items, returning nothing
    <<>>;

encode_data(Scalar,_Level) when is_binary(Scalar) ->
    % For strings, make sure we quote, and convert any non-printable chars
    % to printable. I don't have a better way to do this today - it may not support
    % utf stuff either.
    Escaped = list_to_binary(lists:nth(3,lists:nth(1,io_lib:format("~p", [Scalar])))),

    <<Escaped/binary, "\n">>;

encode_data(Scalar,_Level) when is_integer(Scalar) ->
    % Its a scalar, just return it - convert integer into binary
    Int = list_to_binary(integer_to_list(Scalar)),
    <<Int/binary, "\n">>.


%% @doc Indent based on level
%% @end
indent(Level) ->
    binary:copy(<<"  ">>, Level).
