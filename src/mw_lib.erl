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

-define(BJ_REQUEST_URL, <<"http://localhost:4567/get-incomplete-t2-A">>).
-define(BJ_REQUEST_TIMEOUT, 5000).

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

datetime_to_iso_timestamp({Date, {H, Min, Sec}}) when is_float(Sec) ->
    %% TODO: proper support for milliseconds
    datetime_to_iso_timestamp({Date, {H, Min, round(Sec)}});
datetime_to_iso_timestamp({{Y, Mo, D}, {H, Min, Sec}}) when is_integer(Sec) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Min, Sec]),
    list_to_binary(IsoStr).

%% "bj" / "Bj" does not refer to blowjob but to "bitcoin java service"
bj_req_get_incomplete_t2(GiverPubKey, TakerPubKey, EventPubKey,
                         Value, OwnerOfInputToSign) ->
    QS = cow_qs:qs(
           [
            {<<"giver-pubkey">>, GiverPubKey},
            {<<"taker-pubkey">>, TakerPubKey},
            {<<"event-pubkey">>, EventPubKey},
            {<<"value">>, Value},
            {<<"owner-of-input-to-sign">>, OwnerOfInputToSign}
           ]),
    bj_http_req(<<?BJ_REQUEST_URL/binary, $?, QS/binary>>).

bj_http_req(URL) -> bj_http_req(URL, [], ?BJ_REQUEST_TIMEOUT).
bj_http_req(URL, BodyArgs) -> bj_http_req(URL, BodyArgs, ?BJ_REQUEST_TIMEOUT).
bj_http_req(URL, _BodyArgs, Timeout) ->
    %% TODO: does cowboy has something like this?
    %% Body = mochiweb_util:urlencode(BodyArgs),
    Headers = [], %% [{content_type, "application/x-www-form-urlencoded"}],
    lhttpc:request(URL, post, Headers, [], Timeout).

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
