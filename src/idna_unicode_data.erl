-module(idna_unicode_data).

-export([lookup/1]).
-export([decomposition/1]).

-include("idna_unicode_data.hrl").

lookup(Codepoint) ->
	case find(Codepoint, ?BY_CODE) of
		false -> false;
		Bin -> binary_to_term(Bin)
	end.

decomposition(Key) ->
	case find(Key, ?BY_KEY) of
		false -> false;
		V -> binary_to_list(V)
	end.

find(Key, Tree) when is_list(Key) ->
	find(list_to_binary(Key), Tree);
find(Key, <<_:32, Binary/binary>>) ->
    find_node(byte_size(Key), Key, Binary).

find_node(KeySize, Key, <<HereKeySize:16, HereKey:HereKeySize/binary,
                          BinSizeSmaller:32, _:BinSizeSmaller/binary,
                          ValueSize:32, Value:ValueSize/binary,
                          _/binary>> = Bin) ->
    if
        Key < HereKey ->
            Skip = 6 + HereKeySize,
            << _:Skip/binary, Smaller:BinSizeSmaller/binary, _/binary>> = Bin,
            find_node(KeySize, Key, Smaller);
        HereKey < Key ->
            Skip = 10 + HereKeySize + BinSizeSmaller + ValueSize,
            << _:Skip/binary, Bigger/binary>> = Bin,
            find_node(KeySize, Key, Bigger);
        true ->
            Value
    end;

find_node(_, _, <<>>) ->
    false.
