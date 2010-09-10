%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc YAML encoder/decoder library
%%
%% @end
%% --------------------------
-module(yaml).

-export([encode/1,decode/1]).

%% @doc Encode erlang data structure into YAML text.
%%
%% Returns a YAML encoded document as a binary string. See examples.txt in docs/
%% for examples of the corresponding formats.
%%
%% @end 
encode(Data) ->
    yaml_encoder:encode(Data).
    
%% @doc Decode YAML text into erlang data structure
%%
%% @end 
decode(_Text) ->
    {not_implemented}.
    

