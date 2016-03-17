#!/usr/bin/env escript

-module(mkdata).
-export([main/1]).

-define(URL, "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt").
-define(DATA_FILE, "UnicodeData.txt").
-define(DATA_INCLUDE, "idna_unicode_data.hrl").

-define(string(B), binary_to_list(B)).

fetch() ->
    Req = {?URL, []},
    Opts = [{body_format, binary}, {full_result, false}],
    case httpc:request(get, Req, [], Opts) of
        {ok, {200, Body}} -> {ok, Body};
        {ok, _} = Resp -> {error, Resp};
        Error -> Error
    end.

script_dir() ->
    filename:dirname(escript:script_name()).

store_data(Data) ->
    ScriptDir = script_dir(),
    Path = filename:join([ScriptDir, "priv", ?DATA_FILE]),
    ok = filelib:ensure_dir(Path),
    file:write_file(Path, Data).

process(<<>>, ByCol, ByKey) ->
    {ByCol, ByKey};
process(Data, ByCol, ByKey) ->
    [Line, Rest] = binary:split(Data, <<"\n">>),
    Cols = binary:split(Line, <<";">>, [global]),
    [A, _, _, B, _, C, _, _, _, _, _, _, _, D, _] = Cols,
    ByCol2 = case A of
        <<"">> -> ByCol;
        _ -> gb_trees:enter(A, term_to_binary({?string(B), ?string(C), ?string(D)}), ByCol)
    end,
    ByKey2 = case C of
        <<>> -> ByKey;
        _ -> gb_trees:enter(C, A, ByKey)
    end,
    process(Rest, ByCol2, ByKey2).


from_gb_tree({Count,Node}) when Count =< 16#ffffffff ->
    {_BinSize,IOList} = encode_gb_node(Node),
    erlang:iolist_to_binary([ << Count:32/unsigned >> | IOList ]).

encode_gb_node({Key, Value, Smaller, Bigger}) ->
    {BinSizeSmaller, IOSmaller} = encode_gb_node(Smaller),
    {BinSizeBigger, IOBigger} = encode_gb_node(Bigger),

    KeySize = byte_size(Key),
    ValueSize = byte_size(Value),
    { 2 + KeySize
      + 4 + ValueSize
      + 4 + BinSizeSmaller
      + BinSizeBigger,

      [ << KeySize:16, Key/binary,
           BinSizeSmaller:32 >>, IOSmaller,
        << ValueSize:32, Value/binary >> | IOBigger ] };

encode_gb_node(_Else) ->
    { 0, [] }.


main(_) ->
    ok = application:start(inets),
    io:format("=> Fetch unicode data...~n", []),
    {ok, Data} = fetch(),
    ok = store_data(Data),
    io:format("=> process file...~n", []),
    {ByCol, ByKey} = process(Data, gb_trees:empty(), gb_trees:empty()),
    io:format("=> generate include file...~n", []),
    IncPath = filename:join([script_dir(), "src", ?DATA_INCLUDE]),
    {ok, Fd} = file:open(IncPath, [write, binary, unicode]),
    io:fwrite(Fd, "-define(BY_CODE, ~p).~n-define(BY_KEY, ~p).~n", [from_gb_tree(ByCol), from_gb_tree(ByKey)]),
    file:close(Fd),
    ok.
