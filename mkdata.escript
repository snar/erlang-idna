-module(mkdata).
-export([main/1]).

-define(UNICODE_DATA_FILE, "UnicodeData.txt").
-define(UNICODE_DATA_MODULE, "src/idna_udata.erl").
-define(DATA_URL, "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt").

init_script() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok.

fetch_datafile() ->
    Headers = [{"User-Agent", "erlang-idna/1.0"},
               {"Connection", "close"},
               {"Cache-Control", "no-cache"},
               {"Pragma", "no-cache"}],
    Req = {?DATA_URL, Headers},
    case httpc:request(get, Req, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            file:write_file(?UNICODE_DATA_FILE, Body);
        {ok, {{_, Status, _}, _, _}} ->
            {http_error, Status}
    end.

dehex(Strings) ->
    [erlang:list_to_integer(String, 16) || String <- Strings].

process_code("", _, _, _, Codes) ->
    Codes;
process_code(A0, B0, C0, D0, Codes) ->
    A = list_to_integer(A0, 16),
    B = erlang:list_to_integer(B0),

    C = case C0 of
        [] -> undefined;
        Val ->
            Tokens = string:tokens(Val, " "),
            dehex(case hd(Val) of
                      $< -> tl(Tokens);
                      _ -> Tokens
                  end)
    end,
    D = case D0 of
        [] -> [];
        Hex -> erlang:list_to_integer(Hex, 16)
    end,
    [{A, {B, C, D}} | Codes].

process_key("", _, Keys) ->
    Keys;
process_key(C, A0, Keys) ->
    A = list_to_integer(A0, 16),
    Tokens = string:tokens(C, " "),
    case hd(C) of
        $< -> Keys;
        _ when length(Tokens) =:= 2 -> [{dehex(Tokens), A} | Keys];
        _ -> Keys
    end.

process_file(Fd, Codes, Keys) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            Tokens = re:split(Line, ";", [{return, list}]),
            [A, _, _, B, _, C, _, _, _, _, _, _, _, D, _] = Tokens,
            Codes2 = process_code(A, B, C, D, Codes),
            Keys2 = process_key(C, A, Keys),
            process_file(Fd, Codes2, Keys2);
        eof ->
            _ = file:delete(?UNICODE_DATA_MODULE),
            {ok, Src} = file:open(?UNICODE_DATA_MODULE, [write, append]),
            file:write(Src, "-module(idna_udata).\n"
                            "-export([l/1, k/1]).\n"),
            lists:foreach(fun({K, T}) ->
                io:fwrite(Src, "l(~w) -> ~w;~n", [K, T])
            end, lists:ukeysort(1, Codes)),
            file:write(Src, "l(_) -> false.\n"),
            lists:foreach(fun({K, C}) ->
                io:format("key is ~p~n", [K]),
                io:fwrite(Src, "k(~w) -> ~w;~n", [K, C])
            end, lists:ukeysort(1, Keys)),
            file:write(Src, "k(_) -> false.\n"),
            file:close(Src)
    end.

main(_) ->
    init_script(),
    %% maybe download the datafile
    case filelib:is_file(?UNICODE_DATA_FILE) of
        true -> ok;
        false ->
            ok = fetch_datafile()
    end,

    {ok, Fd} = file:open(?UNICODE_DATA_FILE, [read]),
    process_file(Fd, [], []),
    file:close(Fd),
    ok.
