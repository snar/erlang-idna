-module(idna_unicode_data).

-export([lookup/1]).
-export([decomposition/1]).

-include("idna_unicode_data.hrl").

lookup(Codepoint) ->
	case lists:keyfind(Codepoint, 1, ?BY_CODE) of
		{CodePoint, Val} -> Val;
		false -> false
	end.

decomposition(Key) ->
	case lists:keyfind(Key, 1, ?BY_KEY) of
		{Key, Val} -> Val;
		false -> false
	end.
