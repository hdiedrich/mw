%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% PostgreSQL / epgsql query utility functions
%%% @end
%%% Created : 06 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_lib).

-compile(export_all).
%% API
-export([]). %% TODO: remove export_all and add API exports

-include("log.hrl").
-include_lib("proper/include/proper.hrl").

-define(DEFAULT_REQUEST_TIMEOUT, 5000).

-define(B58_ALPHABET,
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz").
-define(B58_BASE, length(?B58_ALPHABET)).
-define(ALPHABET_CODE(Num), lists:nth((Num rem ?B58_BASE) + 1, ?B58_ALPHABET)).

%%%===========================================================================
%%% API
%%%===========================================================================
hex_to_bin(L) when is_list(L) -> hex_to_bin(binary:list_to_bin(L));
hex_to_bin(B) when is_binary(B) ->
    <<<<(list_to_integer([D,D2], 16))>> || <<D,D2>> <= B>>.

bin_to_hex(B) when is_binary(B) ->
    <<<<(binary:list_to_bin(case length(S = integer_to_list(I, 16)) of
                                1 -> [$0|S];
                                2 -> S
                            end))/bytes>> || <<I>> <= B>>.

dec_b58check(S) when is_list(S)   -> dec_b58check(binary:list_to_bin(S));
dec_b58check(B58) when is_binary(B58) ->
    {Zeroes, Rest} = split_leading_ones_to_zeroes(B58, <<>>),
    PayloadLen = byte_size(Bin = dec_b58(Rest)) - 4,
    <<Payload:PayloadLen/bytes, Hash:4/bytes>> = Bin,
    <<ExpectedHash:4/bytes, _/binary>> =
        double_sha256(<<Zeroes/binary, Payload/binary>>),
    [throw(base58_checksum_validation_failed) || not (ExpectedHash =:= Hash)],
    <<Zeroes/binary, Payload/binary>>.

%% We assume application/version byte was already concatenated with payload
enc_b58check(S) when is_list(S) ->
    enc_b58check(binary:list_to_bin(S));
enc_b58check(B) when is_binary(B) ->
    <<Hash:4/bytes, _/binary>> = double_sha256(B),
    B64 = enc_b58(<<B/binary, Hash/binary>>),
    <<(leading_zeroes_as_ones(B))/binary, B64/binary>>.

leading_zeroes_as_ones(<<0, B/binary>>) ->
    <<"1", (leading_zeroes_as_ones(B))/binary>>;
leading_zeroes_as_ones(_B) -> <<>>.

split_leading_ones_to_zeroes(<<"1", B/binary>>, Acc) ->
    split_leading_ones_to_zeroes(B, <<0, Acc/binary>>);
split_leading_ones_to_zeroes(B, Acc) -> {Acc, B}.


double_sha256(B) when is_binary(B) ->
    crypto:hash(sha256, crypto:hash(sha256, B)).

enc_b58(S) when is_list(S)   -> enc_b58(binary:list_to_bin(S));
enc_b58(<<>>)                -> <<>>;
enc_b58(B) when is_binary(B) -> enc_b58(binary:decode_unsigned(B), <<>>).
enc_b58(Num, Acc) when Num < ?B58_BASE ->
    <<(?ALPHABET_CODE(Num)), Acc/binary>>;
enc_b58(Num, Acc) ->
    enc_b58(Num div ?B58_BASE, <<(?ALPHABET_CODE(Num)), Acc/binary>>).

dec_b58(S) when is_list(S) ->
    dec_b58(binary:list_to_bin(S));
dec_b58(<<>>) -> <<>>;
dec_b58(B) when is_binary(B) ->
    dec_b58(bin_rev(B), 1, 0).

dec_b58(<<>>, _Pow, Num) ->
    binary:encode_unsigned(Num);
dec_b58(<<C, Rest/binary>>, Pow, Num) ->
    case pos_in_list(C, ?B58_ALPHABET) of
        {error, _} ->
            {error, invalid_base58};
        Pos ->
            dec_b58(Rest, Pow * ?B58_BASE, Num + (Pow * (Pos - 1)))
    end.

bin_rev(Bin) -> bin_rev(Bin, <<>>).
bin_rev(<<>>, Acc) -> Acc;
bin_rev(<<H:1/binary, Rest/binary>>, Acc) -> %% binary concatenation is fastest?
    bin_rev(Rest, <<H/binary, Acc/binary>>).

pos_in_list(E, L) when is_list(L) ->
    pos_in_list_aux(E, L, 1).

pos_in_list_aux(E, [E|_], Pos) -> Pos;
pos_in_list_aux(E, [_|T], Pos) -> pos_in_list_aux(E, T, Pos + 1);
pos_in_list_aux(_E, [], _Pos) -> {error, not_in_list}.


datetime_to_iso_timestamp({Date, {H, Min, Sec}}) when is_float(Sec) ->
    %% TODO: proper support for milliseconds
    datetime_to_iso_timestamp({Date, {H, Min, round(Sec)}});
datetime_to_iso_timestamp({{Y, Mo, D}, {H, Min, Sec}}) when is_integer(Sec) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Min, Sec]),
    list_to_binary(IsoStr).

bj_http_req(URL) ->
    bj_http_req(URL, [], ?DEFAULT_REQUEST_TIMEOUT).
bj_http_req(URL, BodyArgs) ->
    bj_http_req(URL, BodyArgs, ?DEFAULT_REQUEST_TIMEOUT).
bj_http_req(URL, _BodyArgs, Timeout) ->
    %% TODO: does cowboy has something like this?
    %% Body = mochiweb_util:urlencode(BodyArgs),
    Headers = [], %% [{content_type, "application/x-www-form-urlencoded"}],
    lhttpc:request(ensure_list(URL), get, Headers, [], Timeout).

ensure_list(B) when is_binary(B) -> binary:bin_to_list(B);
ensure_list(L) when is_list(L) -> L.

set_resp_headers(Headers, Req) ->
    SetHeader = fun({H,V}, ReqAcc) ->
                        cowboy_req:set_resp_header(H, V, ReqAcc)
                end,
    lists:foldl(SetHeader, Req, Headers).

cowboy_req_enable_cors(Req) ->
    Req2 = set_resp_headers([
                             %% TODO: what headers do we need to enable CORS?
                             {<<"Access-Control-Allow-Origin">>,
                              <<"http://127.0.0.1">>
                                  %%<<"*">>
                             },
                             {<<"Access-Control-Allow-Methods">>, <<"GET,POST,OPTIONS">>}
                            ],
                            Req),
    Req2.
%%%===========================================================================
%%% Internal functions
%%%===========================================================================
%%%===========================================================================
%%% Tests
%%%===========================================================================
proper() ->
    ProperOpts =
        [{to_file, user},
         {numtests, 1000}],
    true = proper:quickcheck(prop_base58(), ProperOpts),
    true = proper:quickcheck(prop_hex(), ProperOpts),
    ok.

prop_base58() ->
    ?FORALL(Bin,
            binary(),
            begin
                Bin =:= mw_lib:dec_b58(mw_lib:enc_b58(Bin)),
                Bin =:= mw_lib:dec_b58check(mw_lib:enc_b58check(Bin))
            end).

prop_hex() ->
    ?FORALL(Bin,
            binary(),
            Bin =:= mw_lib:hex_to_bin(mw_lib:bin_to_hex(Bin))).

aes_enc(Key, Plaintext) when byte_size(Key) == 16 ->
    PaddingLen = 16 - (byte_size(Plaintext) rem 16),
    Padding = binary:copy(<<PaddingLen>>, PaddingLen),
    PaddedPlaintext = <<Plaintext/binary, Padding/binary>>,

    Ciphertext = crypto:block_encrypt(aes_cbc128,
                                      Key,
                                      <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
                                      PaddedPlaintext),
    {ok, Ciphertext}.

aes_dec(Key, Ciphertext) when byte_size(Key) == 16 ->
    crypto:block_decrypt(aes_cbc128,
                         Key,
                         <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
                         Ciphertext).
