%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc YAML encoder
%%
%% This code aims for 1.2 compliance of the YAML specification: 
%%
%% http://www.yaml.org/spec/1.2/spec.html
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
    Header = <<"---\n">>,
    Encoded = encode_data(Data),
    <<Header/binary, Encoded/binary>>.
    
%% @doc
%%
%% encode_data(Data)
%% returns: CompleteDocument
%% @end
encode_data([]) ->
    % ran out of list items, returning nothing
    <<"">>;

encode_data({Key,Val}) ->
    % Its a map, grab the key, and re-run encode_data on the value, using
    % the results in your return
    EncodedVal = encode_data(Val),

    <<Key/binary, ": ", EncodedVal/binary>>;

encode_data(Sequence) when is_list(Sequence) ->
    % Its a sequence, so I should iterate across it
    First = lists:nth(1, Sequence),
    EncodedFirst = encode_data(First),
    EncodedSequence = encode_data(lists:delete(First,Sequence)),

    <<EncodedFirst/binary, EncodedSequence/binary>>;

encode_data(Scalar) ->
    % Its a scalar, just return it
    <<Scalar/binary, "\n">>.
