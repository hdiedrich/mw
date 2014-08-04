%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% Contract logic.
%%%
%%% Persistent contract state is maintained in postgres contract tables.
%%% The state is read when generating the contract web page / android view.
%%% The state is modified by events such as:
%%%     * Enter a bet
%%%     * Sign a bet (T2),
%%%     * Cashing out a won bet (T3)
%%%
%%% @end
%%% Created : 05 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_contract).

-compile(export_all).
%% API
-export([]). %% TODO: remove export_all and add API exports

-include("mw.hrl").
-include("mw_contract.hrl").
-include("log.hrl").
-include("mw_api_errors.hrl").

-define(GET(PL), fun(Key) -> proplists:get_value(Key, PL, not_found) end).

-define(DEFAULT_AES_IV, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).
-define(BINARY_PREFIX, <<"A1EFFEC100000000">>).

-define(BJ_T2_MOCKED, false).
-define(BJ_URL_GET_UNSIGNED_T2,     <<"http://orders3.freshworks2.net:4567/get-unsigned-t2">>).
-define(BJ_URL_SUBMIT_T2_SIGNATURE, <<"http://orders3.freshworks2.net:4567/submit-t2-signature">>).

-define(BJ_T3_MOCKED, true).
-define(BJ_URL_GET_UNSIGNED_T3,     <<"http://localhost/get-unsigned-t3">>).
-define(BJ_URL_SUBMIT_T3_SIGNATURES, <<"http:///submit-t3-signatures">>).
-define(BJ_REQUEST_TIMEOUT, 5000).

%%%===========================================================================
%%% Testnet keys for tests / dev / debug
%%%===========================================================================
-define(TEST_EC_EVENT_PRIVKEY, <<"16C746A92F7584013A93004BE8A56709C8CFE0B71E8DEA6DEFC6BE0F7D9CB96C">>).
-define(TEST_EC_EVENT_PUBKEY, <<"025A70A221894F315EBC864292D0DB9F7FF0F817C6D489D176E02BAA7FD4FCE320">>).

-define(TEST_EC_ORACLE_NO_PRIVKEY, <<"7498560BFC3C501C6386A5EA936B548629E37A3E24C88C4FE29E969C83D8CC57">>).
-define(TEST_EC_ORACLE_NO_PUBKEY, <<"039A7D381E13BA745BCA7BE5456E4076A63CA3B54FCB1B2CF7BD8ACA3DACBD06E3">>).

-define(TEST_EC_ORACLE_YES_PRIVKEY, <<"CC74044B7931A452D7039DFE2C9985393843B724DB6251B54DC743E490E7C51C">>).
-define(TEST_EC_ORACLE_YES_PUBKEY, <<"03DC052964F0BCA73CCA109B74CD8F7D2F82C4664AAC142D491DE8B4CC6D244492">>).

%%%===========================================================================
%%% JSON API handlers (called from cowboy callbacks)
%%%===========================================================================
%% Validations throw error so JSON handler can return nice error code / msg
%% Any unhandled error (crash) will return default json error code / msg
enter_contract(ContractId,
               ECPubkey,
               RSAPubkey,
               EncECPrivkey,
               EncRSAPrivkey) ->
    ?info("Handling enter_contract with ContractId: ~p , ECPubkey: ~p"
          "RSAPubkey: ~p EncECprivkey: ~p EncRSaprivkey: ~p",
          [ContractId, ECPubkey, RSAPubkey, EncECPrivkey, EncRSAPrivkey]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),

    %% https://en.bitcoin.it/wiki/Base58Check_encoding
    %% compressed EC pubkeys in base58check encoding is 50 chars
    api_validation(is_binary(ECPubkey) andalso
                   is_binary(catch mw_lib:dec_b58check(ECPubkey)),
                   ?EC_PUBKEY_TYPE),
    api_validation((byte_size(ECPubkey) == 50), ?EC_PUBKEY_LEN),

    api_validation(is_binary(RSAPubkey) andalso
                   %% http://erlang.org/doc/man/public_key.html#pem_decode-1
                   length(catch public_key:pem_decode(RSAPubkey)) == 1,
                   ?RSA_PUBKEY_TYPE),
    %% TODO: what lengths can PEM encoded RSA 2048 pubkeys have?
    % api_validation((byte_size(RSAPubkey) == 902), ?RSA_PUBKEY_LEN),

    %% TODO: validation of encrypted privkeys in hex, what is length?

    ok = do_enter_contract(ContractId,
                           ECPubkey,
                           RSAPubkey,
                           EncECPrivkey,
                           EncRSAPrivkey),
    [{<<"success-message">>, <<"ok">>}].

submit_t2_signature(ContractId, ECPubkey, T2Signature) ->
    ?info("Handling submit_signed_t2_hash with ContractId: ~p , ECPubkey: ~p, "
          "T2Signature: ~p",
          [ContractId, ECPubkey, T2Signature]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),

    api_validation(is_binary(ECPubkey) andalso
                   is_binary(catch mw_lib:dec_b58check(ECPubkey)),
                   ?EC_PUBKEY_TYPE),
    api_validation((byte_size(ECPubkey) == 50), ?EC_PUBKEY_LEN),

    api_validation(is_binary(catch mw_lib:hex_to_bin(T2Signature)),
                   ?SIGNATURE_TYPE),
    api_validation(bitcoin_signature_der(mw_lib:hex_to_bin(T2Signature)),
                   ?SIGNATURE_TYPE),

    ok = do_submit_t2_signature(ContractId, ECPubkey, T2Signature),
    [{<<"success-message">>, <<"ok">>}].

get_t3_for_signing(ContractId, ToAddress) ->
    ?info("Handling get_t3_for_signing with ContractId: ~p , ToAddress: ~p ",
          [ContractId, ToAddress]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),

    api_validation(is_binary(ToAddress) andalso
                   is_binary(catch mw_lib:dec_b58check(ToAddress)),
                   ?ADDRESS_TYPE),
    api_validation((byte_size(ToAddress) >= 27) andalso
                   (byte_size(ToAddress) =< 34),
                   ?ADDRESS_LEN),

    ResultProps = do_get_t3_for_signing(ContractId, ToAddress),
    [{<<"success-message">>, <<"ok">>}] ++ ResultProps.

submit_t3_signatures(ContractId, T3Raw, T3Signature1, T3Signature2) ->
    ?info("Handling submit_t3_signatures with ContractId: ~p "
          ", T3Signature1: ~p, T3Signature2: ~p",
          [ContractId, T3Signature1, T3Signature2]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),

    api_validation(is_binary(catch mw_lib:hex_to_bin(T3Signature1)),
                   ?SIGNATURE_TYPE),
    api_validation(bitcoin_signature_der(mw_lib:hex_to_bin(T3Signature1)),
                   ?SIGNATURE_TYPE),

    api_validation(is_binary(catch mw_lib:hex_to_bin(T3Signature2)),
                   ?SIGNATURE_TYPE),
    api_validation(bitcoin_signature_der(mw_lib:hex_to_bin(T3Signature2)),
                   ?SIGNATURE_TYPE),

    %% TODO: return more stuff in JSON response?
    _JSONRes = do_submit_t3_signatures(ContractId,
                                       T3Raw, T3Signature1, T3Signature2),
    [{<<"success-message">>, <<"ok">>}].

%%%===========================================================================
%%% Internal Erlang API (e.g. called by cron jobs / internal Mw services) but
%%% which may be exposed as JSON API later on
%%%===========================================================================
%% For MVP #2 this can be used for pages: prep, pend, sign and status
get_contract_t2_state(Id) ->
    {ok, Info}  = get_contract_info(Id),
    GetInfo     = ?GET(Info),
    History     = GetInfo("history"),
    EventPubkey = GetInfo("event_pubkey"),
    GiverPubkey = GetInfo("giver_ec_pubkey"),
    TakerPubkey = GetInfo("taker_ec_pubkey"),
    Value       = <<"20000">>,

    %% TODO: only return the strictly needed encrypted
    %% privkeys instead of all of them
    %% TODO: for now we simplify flow and assume both have sent T1
    %% when we get first T2 from Bj
    case {contract_event_happened(History, ?STATE_DESC_GIVER_ENTERED),
          contract_event_happened(History, ?STATE_DESC_TAKER_ENTERED),
          contract_event_happened(History, ?STATE_DESC_GIVER_T1),
          contract_event_happened(History, ?STATE_DESC_TAKER_T1)} of
        {true, true, false, false} ->
            %% call Bj to see if t1 outputs are available as t2 inputs
            ReqRes = bj_req_get_unsigned_t2(GiverPubkey, TakerPubkey,
                                            EventPubkey, Value),
            ?info("bj_req_get_unsigned_t2: ~p", [ReqRes]),
            GetRes = ?GET(ReqRes),
            case GetRes(<<"error-message">>) of
                not_found ->
                    T2SigHashInput0 = GetRes(<<"t2-sig-hash-input0">>), %% giver
                    T2SigHashInput1 = GetRes(<<"t2-sig-hash-input1">>), %% taker
                    T2Raw = GetRes(<<"t2-raw">>),
                    T2Hash = GetRes(<<"t2-hash">>),
                    ok = mw_pg:update_contract_t2(Id,
                                                  T2SigHashInput0,
                                                  T2SigHashInput1,
                                                  T2Raw, T2Hash),
                    ok = mw_pg:insert_contract_event(Id, ?STATE_DESC_GIVER_T1),
                    ok = mw_pg:insert_contract_event(Id, ?STATE_DESC_TAKER_T1),
                    {ok, NewInfo} = get_contract_info(Id),
                    {ok, NewInfo};
                _ErrorMsg ->
                    %% No t2 from Bj: no T1 outputs available; unchanged state
                    {ok, Info}
            end;
        {true, false, false, false} ->
            %% MVP #2 state: only default giver has entered, taker has not
            {ok, Info};
        {true, true, true, true} ->
            %% waiting for signatures; unchanged state
            {ok, Info}
    end.

get_contract_info(Id) ->
    {ok, MatchNo, Headline, Desc, Outcome,
     EventPubkey, GiverECPubkey, TakerECPubkey,
     GiverEncECPrivkey, TakerEncECPrivkey,
     GiverEncRSAPrivkey, TakerEncRSAPrivkey,
     EncEventKeyYes, EncEventKeyNo,
     T2SigHashInput0, T2SigHashInput1, T2Raw, T2Hash,
     FormatedEvents} =
        mw_pg:select_contract_info(Id),
    %% Some of these fields have same name as Postgres column names, but we
    %% avoid the temptation of using them directly to have separation between
    %% postgres schema and JSON API schema
    {ok, [
          {"match_no", MatchNo},
          {"headline", Headline},
          {"desc", Desc},
          {"outcome", Outcome},
          {"event_pubkey", EventPubkey},
          {"giver_ec_pubkey", GiverECPubkey},
          {"taker_ec_pubkey", TakerECPubkey},
          {"giver_enc_ec_privkey", GiverEncECPrivkey},
          {"taker_enc_ec_privkey", TakerEncECPrivkey},
          {"giver_enc_rsa_privkey", GiverEncRSAPrivkey},
          {"taker_enc_rsa_privkey", TakerEncRSAPrivkey},
          {"event_key_enc_with_oracle_yes_and_giver_keys", EncEventKeyYes},
          {"event_key_enc_with_oracle_no_and_taker_keys", EncEventKeyNo},
          {"t2_sighash_input_0", T2SigHashInput0},
          {"t2_sighash_input_1", T2SigHashInput1},
          {"t2_hash", T2Hash},
          {"t2_raw", T2Raw},
          {"history", lists:map(fun({Timestamp, Event}) ->
                                        [{"timestamp", Timestamp},
                                         {"event", Event}]
                                end, FormatedEvents)}
         ]}.

create_contract(EventId) ->
    {ok, ContractId} = mw_pg:insert_contract(EventId),
    ok = mw_pg:insert_contract_event(ContractId, ?STATE_DESC_CREATED),
    {ok, ContractId}.

clone_contract(Id) ->
    {ok, NewId} = mw_pg:clone_contract(Id),
    ok = mw_pg:insert_contract_event(NewId, ?STATE_DESC_CLONED),
    [{"new_contract", NewId}].

create_oracle_keys(NoPubkey, NoPrivkey, YesPubkey, YesPrivkey) ->
    %% Validations for EC keys
    %%api_validation(is_binary(NOPubkey), ?EC_PUBKEY_TYPE),
    %%api_validation(is_binary(YESPubkey), ?EC_PUBKEY_TYPE),
    %%api_validation((byte_size(NOPubkey) == 130), ?PUBKEY_LEN),
    %%api_validation((byte_size(YESPubkey) == 130), ?PUBKEY_LEN),
    {ok, Id} = mw_pg:insert_oracle_keys(NoPubkey, NoPrivkey,
                                        YesPubkey, YesPrivkey),
    {ok, Id}.

create_event(MatchNum, Headline, Desc, OracleKeysId,
             EventPrivkey, EventPubkey) ->
    {ok, NoPubkeyPEM, YesPubkeyPEM} = mw_pg:select_oracle_keys(OracleKeysId),
    {ok, NoPubkey}  = pem_decode_bin(NoPubkeyPEM),
    {ok, YesPubkey} = pem_decode_bin(YesPubkeyPEM),
    EventPrivkeyEncWithOracleNoKey =
        hybrid_aes_rsa_enc(EventPrivkey, NoPubkey),
    EventPrivkeyEncWithOracleYesKey =
        hybrid_aes_rsa_enc(EventPrivkey, YesPubkey),
    {ok, EventId} =
        mw_pg:insert_event(MatchNum, Headline, Desc, OracleKeysId, EventPubkey,
                           EventPrivkeyEncWithOracleNoKey,
                           EventPrivkeyEncWithOracleYesKey),
    {ok, EventId}.


add_event_outcome(EventId, Outcome) when (Outcome =:= true) orelse
                                         (Outcome =:= false) ->
    ok = mw_pg:update_event(EventId, Outcome),
    {ok, ContractIds} = mw_pg:select_contracts_of_event(EventId),
    Event = ?STATE_DESC_EVENT_OUTCOME_HAPPENED,
    [ok = mw_pg:insert_contract_event(Id, Event) || Id <- ContractIds],
    ok.

add_contract_outcome(ContractId, Outcome) when (Outcome =:= true) orelse
                                               (Outcome =:= false) ->
    {ok, EventId} = mw_pg:select_event_id(ContractId),
    ok = mw_pg:update_event(EventId, Outcome),
    {ok, ContractIds} = mw_pg:select_contracts_of_event(EventId),
    Event = ?STATE_DESC_EVENT_OUTCOME_HAPPENED,
    [ok = mw_pg:insert_contract_event(Id, Event) || Id <- ContractIds],
    ok.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
create_contract_event(Event) ->
    ok = mw_pg:insert_contract_event(Event),
    ok.

%% TODO: think about abstraction concerns regarding matching on postgres 'null'
%% TODO: this assumes giver always enters first
%% TODO: generalize
do_enter_contract(ContractId,
                  ECPubkey,
                  RSAPubkeyPEM,
                  EncECPrivkey,
                  EncRSAPrivkey) ->
    {ok, RSAPubkey} = pem_decode_bin(RSAPubkeyPEM),
    {YesOrNo, GiverOrTaker, _GiverKey} =
        case mw_pg:select_contract_ec_pubkeys(ContractId) of
            {ok, null, null}          -> {yes, giver, nope};
            {ok, GiverECPubkey, null} -> {no, taker, GiverECPubkey};
            {ok, _GiverECPubkey, _TakerECPubkey} ->
                ?API_ERROR(?CONTRACT_FULL);
            {error,{ok,[]}} ->
                ?API_ERROR(?CONTRACT_NOT_FOUND)
        end,
    {ok, EncEventKey} =
        mw_pg:select_enc_event_privkey(ContractId, YesOrNo),
    DoubleEncEventKey = hybrid_aes_rsa_enc(EncEventKey, RSAPubkey),
    ok = mw_pg:update_contract_enter(ContractId,
                                     GiverOrTaker,
                                     ECPubkey,
                                     RSAPubkeyPEM,
                                     EncECPrivkey,
                                     EncRSAPrivkey,
                                     DoubleEncEventKey),
    EnteredEvent = case GiverOrTaker of
                       giver -> ?STATE_DESC_GIVER_ENTERED;
                       taker -> ?STATE_DESC_TAKER_ENTERED
                   end,
    ok = mw_pg:insert_contract_event(ContractId, EnteredEvent),
    ok.

do_submit_t2_signature(ContractId, ECPubkey, T2Signature) ->
    {ok, Info}  = get_contract_info(ContractId),
    GetInfo     = ?GET(Info),
    GiverPubkey = GetInfo("giver_ec_pubkey"),
    TakerPubkey = GetInfo("taker_ec_pubkey"),

    %% validate contract event states? e.g. duplicated signing reqs
    GiverOrTaker = case ECPubkey of
                       GiverPubkey -> <<"giver">>;
                       TakerPubkey -> <<"taker">>;
                       _           -> ?API_ERROR(?EC_PUBKEY_MISMATCH)
                   end,
    T2Raw = GetInfo("t2_raw"),
    ReqRes =
        bj_req_submit_t2_signature(ECPubkey, T2Signature, T2Raw, GiverOrTaker),
    NewT2     = proplists:get_value("t2-raw-partially-signed", ReqRes),
    NewT2Hash = proplists:get_value("new-t2-hash", ReqRes),
    ok = mw_pg:update_contract_t2(ContractId, NewT2, NewT2Hash),
    SignEvent = case GiverOrTaker of
                       <<"giver">> -> ?STATE_DESC_GIVER_SIGNED_T2;
                       <<"taker">> -> ?STATE_DESC_TAKER_SIGNED_T2
                   end,
    ok = mw_pg:insert_contract_event(ContractId, SignEvent),
    case proplists:get_value("t2-broadcasted", ReqRes) of
        "true" ->
            ok = mw_pg:insert_contract_event(ContractId, ?STATE_DESC_T2_BROADCASTED);
        "false" ->
            ?API_ERROR(?EC_PUBKEY_MISMATCH)
    end,
    ok.

do_get_t3_for_signing(ContractId, ToAddress) ->
    {ok, Info}  = get_contract_info(ContractId),
    GetInfo     = ?GET(Info),
    History     = GetInfo("history"),
    %% TODO: only return the strictly needed encrypted
    %% privkeys instead of all of them
    case {contract_event_happened(History, ?STATE_DESC_T2_BROADCASTED),
          contract_event_happened(History, ?STATE_DESC_EVENT_OUTCOME_HAPPENED),
          contract_event_happened(History, ?STATE_DESC_T3_BROADCASTED)} of
        {false, false, false} ->
            %% If called before T3 can be created
            ?API_ERROR(?CONTRACT_T2_NOT_COMPLETE);
        {true, false, false} ->
            %% Event outcome has not happened yet
            ?API_ERROR(?NO_EVENT_OUTCOME);
        {true, true, false} ->
            %% T2 broadcasted, event outcome happened, time to grab T3 from Bj.
            T2Hash = GetInfo("t2_hash"),
            ReqRes = bj_req_get_unsigned_t3(T2Hash, ToAddress),
            T3Sighash  = proplists:get_value("t3-sighash", ReqRes),
            T3Hash  = proplists:get_value("t3-hash", ReqRes),
            T3Raw = proplists:get_value("t3-raw",  ReqRes),
            %% TODO: here we act as oracle, sending oracle yes/no privkey
            %% depending on event outcome. In future, this could be done by
            %% external oracle(s) and we would instead grab it from e.g. their
            %% website or somesuch
            {YesOrNo, EventKeyName} =
                case GetInfo("outcome") of
                    true ->
                        {yes, "event_key_enc_with_oracle_yes_and_giver_keys"};
                    false ->
                        {no, "event_key_enc_with_oracle_no_and_taker_keys"}
                end,
            {ok, OPK} = mw_pg:select_oracle_privkey(ContractId, YesOrNo),
            EncEventKey = mw_lib:bin_to_hex(GetInfo(EventKeyName)),
            [
             %% TODO: improve this mapping and also what enc keys are returned
             {"giver_enc_ec_privkey", GetInfo("giver_enc_ec_privkey")},
             {"taker_enc_ec_privkey", GetInfo("taker_enc_ec_privkey")},
             {"giver_enc_rsa_privkey", GetInfo("giver_enc_rsa_privkey")},
             {"taker_enc_rsa_privkey", GetInfo("taker_enc_rsa_privkey")},
             {"oracle_privkey", base64:encode(OPK)}, %% Avoids line breaks in JS
             {"enc_event_privkey", EncEventKey},
             {"t3-sighash", T3Sighash},
             {"t3-hash", T3Hash},
             {"t3-raw", T3Raw}
            ];
        {true, true, true} ->
            %% T3 broadcasted: end state of contract.
            ?API_ERROR(?CONTRACT_FINISHED)
    end.

do_submit_t3_signatures(ContractId, T3Raw, T3Signature1, T3Signature2) ->
    ReqRes        = bj_req_submit_t3_signatures(T3Raw,
                                                T3Signature1, T3Signature2),
    NewT3Hash     = proplists:get_value("new-t3-hash", ReqRes),
    NewT3Raw      = proplists:get_value("new-t3-raw", ReqRes),
    T3Broadcasted = proplists:get_value("t3-broadcasted", ReqRes),
    case T3Broadcasted of
        "true" -> continue;
        _      -> ?API_ERROR(?T3_NOT_BROADCASTED)
    end,
    ok = mw_pg:insert_contract_event(ContractId, ?STATE_DESC_SIGNED_T3),
    [
     {"new-t3-hash", NewT3Hash},
     {"new-t3-raw", NewT3Raw}
    ].

api_validation(false, APIError) -> ?API_ERROR(APIError);
api_validation(true, _)         -> continue.

contract_event_happened(History, Event) ->
    %% contract history is list of list of two two tuples; timestamp and event
    case lists:filter(fun([_,{_, E}]) when E =:= Event -> true;
                         (_) -> false end,
                      History) of
        %% TODO: re-enable check for duplicated events once Bj mocks do not
        %% create duplicated events
        %% Explicit match instead of e.g. lists:any/2
        %% to enforce no duplicated events
        []  -> false;
        [_] -> true;
        _   -> true
    end.

bj_req_get_unsigned_t2(GiverPubkey0, TakerPubkey0, EventPubkey0,
                       Value) ->
    GiverPubkey = mw_lib:bin_to_hex(mw_lib:dec_b58check(GiverPubkey0)),
    TakerPubkey = mw_lib:bin_to_hex(mw_lib:dec_b58check(TakerPubkey0)),
    EventPubkey = mw_lib:bin_to_hex(mw_lib:dec_b58check(EventPubkey0)),
    QS = cow_qs:qs(
           [
            {<<"giver-pubkey">>, GiverPubkey},
            {<<"taker-pubkey">>, TakerPubkey},
            {<<"event-pubkey">>, EventPubkey},
            {<<"value">>, Value}
           ]),
    %% TODO: parse response to proplist
    {ok, Res} =
        case ?BJ_T2_MOCKED of
            true ->
                {ok,
                 [
                  {"t2-sighash-input-0", "A1EFFEC100000000FF01"},
                  {"t2-sighash-input-1", "A1EFFEC100000000FF02"},
                  {"t2-raw", "A1EFFEC100000000FF03"},
                  {"t2-hash", "A1EFFEC100000000FF33"}
                 ]
                 };
            false ->
                URL = <<?BJ_URL_GET_UNSIGNED_T2/binary,
                        $?, QS/binary>>,
                %% ?info("HURR URL: ~p", [URL]),
                {ok, {{_StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} =
                    mw_lib:bj_http_req(URL, [], 5000),
                %% ?info("HURR: ~p", [ResponseBody]),
                {PL} = jiffy:decode(ResponseBody),
                {ok, PL}
        end,
    ?info("HURR Res: ~p", [Res]),
    Res.

bj_req_submit_t2_signature(ECPubkey, T2Signature, T2Raw, GiverOrTaker) ->
    QS = cow_qs:qs(
           [
            {<<"t2-signature">>, T2Signature},
            {<<"t2-raw">>, T2Raw},
            {<<"pubkey">>, ECPubkey},
            {<<"sign-for">>, GiverOrTaker}
           ]),
    %% TODO: parse response to proplist
    {ok, Res} =
        case ?BJ_T2_MOCKED of
            true ->
                {ok,
                 [
                  {"new-t2-hash", "A1EFFEC100000000FF04"},
                  {"t2-raw-partially-signed", "A1EFFEC100000000FF05"},
                  {"t2-broadcasted", "true"}
                 ]
                 };
            false ->
                URL = <<?BJ_URL_SUBMIT_T2_SIGNATURE/binary,
                        $?, QS/binary>>,
                ?info("HURR URL: ~p", [URL]),
                {ok, {{_StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} =
                    mw_lib:bj_http_req(URL, [], 5000),
                {PL} = jiffy:decode(ResponseBody),
                {ok, PL}
        end,
    Res.

bj_req_get_unsigned_t3(T2Hash, ToAddress) ->
    QS = cow_qs:qs(
           [
            {<<"t2-hash">>, T2Hash},
            {<<"to-address">>, ToAddress}
           ]),
    %% TODO: parse response to proplist
    {ok, Res} =
        case ?BJ_T3_MOCKED of
            true ->
                {ok,
                 [
                  {"t3-sighash", "A1EFFEC100000000FF06"},
                  {"t3-hash", "A1EFFEC100000000FF07"},
                  {"t3-raw", "A1EFFEC100000000FF08"}
                 ]
                 };
            false ->
                mw_lib:bj_http_req(<<?BJ_URL_GET_UNSIGNED_T3/binary,
                                     $?, QS/binary>>, [], 5000)
        end,
    Res.

bj_req_submit_t3_signatures(T3Raw, T3Signature1, T3Signature2) ->
    QS = cow_qs:qs(
           [
            {<<"t3-raw">>, T3Raw},
            {<<"t3-signature1">>, T3Signature1},
            {<<"t3-signature2">>, T3Signature2}
           ]),
    %% TODO: parse response to proplist
    {ok, Res} =
        case ?BJ_T3_MOCKED of
            true ->
                {ok,
                 [
                  {"new-t3-hash", "A1EFFEC100000000FF09"},
                  {"new-t3-raw", "A1EFFEC100000000FF10"},
                  {"t3-broadcasted", "true"}
                 ]
                 };
            false ->
                mw_lib:bj_http_req(<<?BJ_URL_SUBMIT_T3_SIGNATURES/binary,
                                     $?, QS/binary>>, [], 5000)
        end,
    Res.

bitcoin_signature_der(<<48,_,2,RL,_R:RL/bytes,2,SL,_S:SL/bytes>>) -> true;
bitcoin_signature_der(_Bin)                                       -> false.

pem_decode_bin(Bin) ->
    [Entry] = public_key:pem_decode(Bin),
    Key = public_key:pem_entry_decode(Entry),
    {ok, Key}.

rsa_key_from_file(PrivPath) ->
    AbsPath = filename:join(code:priv_dir(middle_server), PrivPath),
    {ok, Bin} = file:read_file(AbsPath),
    %%{ok, Key} = pem_decode_bin(Bin),
    Bin.

%% TODO: this assumes RSA 2048.
hybrid_aes_rsa_enc(Plaintext, RSAPubkey) ->
    %% TODO: validate entropy source! We may want to block for /dev/random
    %% to be sure the AES key is cryptographically strong.
    AESKey = crypto:strong_rand_bytes(16),
    %% PKCS #7 padding; value of each padding byte is the integer representation
    %% of the number of padding bytes. We align to 16 bytes.
    PaddingLen = 16 - (byte_size(Plaintext) rem 16),
    Padding = binary:copy(<<PaddingLen>>, PaddingLen),
    PaddedPlaintext = <<Plaintext/binary, Padding/binary>>,
    Ciphertext = crypto:block_encrypt(aes_cbc128, AESKey,
                                      ?DEFAULT_AES_IV, PaddedPlaintext),
    %% Use OAEP as it's supported by Tom Wu's rsa2.js (RSADecryptOAEP)
    %% http://en.wikipedia.org/wiki/Optimal_Asymmetric_Encryption_Padding
    {_RecordName, Modulus, Exponent} = RSAPubkey,
    EncAESKey = crypto:public_encrypt(rsa, AESKey, [Exponent, Modulus],
                                      rsa_pkcs1_oaep_padding),
    %% Distinguishable prefix to identify the binary in case it's on the loose
    <<(mw_lib:hex_to_bin(?BINARY_PREFIX))/binary,
      EncAESKey/binary,
      Ciphertext/binary>>.

%%%===========================================================================
%%% Dev / Debug / Manual Tests
%%%===========================================================================
%% mw_contract:manual_test_1().
manual_test_1() ->
    {ok, _Id} =
        create_oracle_keys(
          rsa_key_from_file("test_keys/oracle_keys1/oracle_no_pubkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys1/oracle_no_privkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys1/oracle_yes_pubkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys1/oracle_yes_privkey.pem")),

    {ok, _Id2} =
        create_oracle_keys(
          rsa_key_from_file("test_keys/oracle_keys2/oracle_no_pubkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys2/oracle_no_privkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys2/oracle_yes_pubkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys2/oracle_yes_privkey.pem")),

    {ok, _Id3} =
        create_oracle_keys(
          rsa_key_from_file("test_keys/oracle_keys3/oracle_no_pubkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys3/oracle_no_privkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys3/oracle_yes_pubkey.pem"),
          rsa_key_from_file("test_keys/oracle_keys3/oracle_yes_privkey.pem")),

    {ok, _} = create_event(1, "Brazil beats Croatia", "More foo info", 1,
                           mw_lib:hex_to_bin(?TEST_EC_EVENT_PRIVKEY),
                           mw_lib:hex_to_bin(?TEST_EC_EVENT_PUBKEY)),

    {ok, _} = create_event(1, "Croatia beats Brazil", "More foo info", 2,
                           mw_lib:hex_to_bin(?TEST_EC_EVENT_PRIVKEY),
                           mw_lib:hex_to_bin(?TEST_EC_EVENT_PUBKEY)),

    {ok, _} = create_event(1, "Match is invaded by aliens", "More foo info", 3,
                           mw_lib:hex_to_bin(?TEST_EC_EVENT_PRIVKEY),
                           mw_lib:hex_to_bin(?TEST_EC_EVENT_PUBKEY)),


    ok.

manual_test_2() ->
    {ok, _} = create_contract(1),
    ok.

decryption_test(_UserECPrivkey, UserRSAPrivkeyPEM,
                OraclePrivkeyHex, EncEventKey) ->
    {ok, UserRSAPrivkey} = pem_decode_bin(UserRSAPrivkeyPEM),
    {ok, OraclePrivkey} = pem_decode_bin(mw_lib:hex_to_bin(OraclePrivkeyHex)),

    <<_Prefix:8/binary, EncAESKey:256/binary, CipherText1/binary>> =
        mw_lib:hex_to_bin(EncEventKey),
    AESKey = public_key:decrypt_private(EncAESKey, UserRSAPrivkey),
    Plaintext1 = crypto:block_decrypt(aes_cbc128,
                                      AESKey,
                                      ?DEFAULT_AES_IV,
                                      CipherText1),
    %% Yo dawg
    <<_Prefix:8/binary,
      EncAESKey2:256/binary,
      CipherText2/binary>> = remove_pkcs_7_padding(Plaintext1),
    AESKey2 = public_key:decrypt_private(EncAESKey2, OraclePrivkey),
    Plaintext2Padded = crypto:block_decrypt(aes_cbc128,
                                            AESKey2,
                                            ?DEFAULT_AES_IV,
                                            CipherText2),
    Plaintext2 = remove_pkcs_7_padding(Plaintext2Padded),
    ?info("Plaintext2: ~p, is base58check: ~p",
          [Plaintext2, is_binary(catch mw_lib:dec_b58check(Plaintext2))]),
    ok.

log(Call, Param, Expect) ->
    ?info(
        list_to_binary(
            io_lib:format("-----------------------~ncall: ~p~nparameter: ~p~nexpected response: ~p~n",
                [Call, Param, Expect]))).

remove_pkcs_7_padding(Bin) ->
    Len = byte_size(Bin),
    PaddingLen = binary:last(Bin),
    binary:part(Bin, {0, Len - PaddingLen}).

%% mw_contract:decryption_test_1().
decryption_test_1() ->
    mw_contract:decryption_test(
      <<"5KDPmciYDKwsfY6nkKrsQvS6aFMRPaBMHGwY5EMRu41keyukhHU">>,
      <<"-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAte5anzaNkLHc5X81utnpaeGQka3R6mgc3rT+M8qXNax9NKGJ\nEXqmCHG+Kbbx0CqvdL0wxyfg7tmiF+EW5pqjlSWwqBPvYUJYWiT1z/sscAnoM1zr\nL3M6NSWhztTj97+Pfn5RdehhMJ2VmnxczJxPuncrkH1UoSS1cduiBplDAMYsk8eW\nuwVpW8RDgAooAm2j2GqJWpi+y2qv4RaM59jY7dPgudQPPdCm7u2l4YK1tesMtsAZ\na4fWdPwW3JMuJ7SSf9V7NshhH78BixOcZA6lZm7vldmXYaFRS9sVMZ65S8S9UYGB\nbO3RmpOOYwT0GCFS7O6/WUWtvoxaYbAXRaUb9QIDAQABAoIBAFFPJ71EelySwXDo\nO7E4tvMVVrFlCA5FXbHDHmEzSAU7A/JDx6jCMfZQL3chRk2M5kG8VFKN5h+ZsLIP\nbVa/AiEkaSGVV0UWi7ouDpZXYXLpWpeqDYp1ayxZl0mDKCePu6gC+JFDvDwoEbar\noiXoLlHd9OBswygJ6fXq/S0JzkJHjo+hUOzFI6iZtM4IfpfFjXU/1wU/eSjWVxzr\napelnSz5CsV/kouHnpD9tGudnBKOilcWnYmhr1WsTI2xiNZWKkxKylj+zj9Fkp+v\nHOYR8UCTuWVPKLS6JaPd2TCAMagPjTB9sS13HhJZguFm14PgVQtcOUSfkpIpXOlM\nA4v5vDkCgYEA4Nad92oJ3N80MPEdSPqaC2pfDc1NVOjY72XrTLOcnTYZ2HWTg8/6\nKKdCV0ipmVo/n/wH+vkM54Njtj/dQHdGUeozK89e1UG0vQHXScS3ZtaAVZlF1msC\niFx9sS9HuJI0JFNIQPNTEq7Fs4ZLfJdhcGgeMz6VAjRL7AHo+ZLH4U8CgYEAzyVd\nODLqcwCh7NaDdYjCrmDRf3VBOBAyjeHGKoTjTa6v+7p8gH6DQDyehDuXDiCrA8xH\nVIMF/62VElXKwqvOnbLMm2kUkDycSDseQFzSFbk6x/xlw0WSVe56T7qWnM4KkJEw\n3TqVzYoTNM7mYiK5VtGB5cnjdSS57YVD3bQBtXsCgYEAqFFCtMPXWlhmU/VNPSMO\nS1l4i3aUW+ps7NtZyXP03ORxeNCcfGMoHWMJkRo+jSU42GXu+32SoYaFERzCX85r\nAEvZvwRhNDkaOxyztO/ldMFEFdDGrXwyyy6ikhCZGp2pF0CZqLYADM52Bq2UuVMC\niQoJAcfp+Vp5M9dCOAQgSpUCgYAsY7liPab4Ff6dHir1mOT/MUgzpBDhzrbGqMcq\nfWeoUfLsYr4jWvkNXvApLgvkvyNmoPP4LEuwyqXTVAcrSF3ydUpbU11Qu2xSHjkR\nWdK7TQJHsNKt0c7WE1CqnTWBTLX+3N43ykIn1ZrgCiZciUxmSmcnsufHRqOBPrmY\nXOaw2QKBgQCZpCI52nKRBaDB3cfKS2gW+M2hBBHVd/yMg5l+QI3nO1qqS3oRAZli\nssOjdcizgf5lJIosmjr3mmLtfBg6vWah0DmHaJh8NC58c7X8//1sHmd7Pv/r1Jpc\nrQabJyiQFULwgUJTmNUlY70Ii+q4Yk25F7S83s+ld3kXNbs5/d6dUA==\n-----END RSA PRIVATE KEY-----">>,
      <<"2d2d2d2d2d424547494e205253412050524956415445204b45592d2d2d2d2d0a4d4949456f77494241414b43415145417a51463646714845523242684444637643412b66496375472b416f62442b7865613147625753786845385158354455650a5667413531447951484b424d7031636a54783530486347706f4d67746e48663057435a306d7a4f665a623462656835366c7035754a62787a6849664a6d3436530a6541557746306f34357a46566d75556b5777726463524a4842496968344b686d444c69456a5334334978436e764d797058532f567a6c4b2f4564386c30376b410a79384a62482f4e6149493739545361567772663333796a474d4261703333695251597a464a744c44384d424b77714165306931596a584b6238666c306c762b710a386b6c38534a48696e4863747459785074667a5978545173796b6658644e786b5445687a324c757552704244764c44324a746e45416b743352384c707a6230720a7956342f497047594b58314436664c764f772f6c6f2f6d2f55654541583578726849352f6d77494441514142416f49424147474445315353693638377938326f0a72676c536a4f6e68536f6a50486349384e306b7133773753696a614272712b52434b58316f3477745a6f7348514937555a756f715853634f5343592f4d5672630a762b2f7945734d4545677975475a6e447836472f4b4474773453326e2f53437439304e356a4439337759744f7278356e59687967465762455039324c4e42794e0a5855557475336341425a2f374f7834563533563433475577664d76396b7277364641316a2f4e744342615a47576a48496c543445314542722b774866545971520a513745736633693241632b3848337a34764e3261693141485a4f4564765a636b38546355316c6f5a617a6a667851694d7378355a4a6c395a2b68443730682b6b0a6462366d3963597677432f484b7550732b64692f395272537237367942374c4963664a532b474743315272684b4c386d353841364a595a51495150694130396c0a5179364e2f544543675945412b31453753796a576a543377626e51466856622b716c6658554b5a4f663242547a354c626a784e472b45503770484d42317074540a4d4d3155656d7368456d6e436f7a7930646e354c6d767156616f6265575548714d4a444b63663735354e6b687355513667734a6e7578394d366f4675795975750a4150614b733231737843382f4f716162586d50783462657665637754665848555545485567364e4a4337366f47306267526653554b486b4367594541304e4e580a6a37643377553754453635356e586c53316961736f6e74486a615376642b304d6b2b74776e7a454761394e32764153634e43786155336c5764782f51717739630a6142564e3456413479516e46665344645276442f61667376664a6c69773063726a6c4b31332f595253434f4331676c4d58452f657654416d434e78333535536d0a4d384e366a4d50724a377a486d6d5a5a352b6e65496f734136467a39434d71327961444243374d43675945413053436b62594f6248636448396a47586b6f42300a78447a3655414e44324e7a496754646365713934354168324d7a3738625a2b73317a5256737454496e5844695a5547546a664c4561543952374759305369502b0a673946496c526d75796f386761556b30517a4551626e6136593332334f6747416c773652466f50632b56657a6a744f72414232383371346957414449797839590a7044785459674d544f724735593546542b514a7638306b43675941747146653373337836686e702b44694c472f48545248386d564350452f4665664c683865390a4f376633426d674b415555766e4b3575646467563869796d6851314271573068304b366a65622f41724f4e4346594a723956745331376d744f3367746a4130490a354371683268497a504a68707063544e566955304a6966617669463150376f4c4455317049356a78716b50574456545138514242657a54716d524931377553430a4e79536549774b4267465051715941394a4a77363668615230634d4b59356d424b3039396a2b4a6b6464537031464a434345693569724d7062764379374d68780a76336c4348705a6b42616d435a3942484f7a63785730532b4e584f54614464536e764c6b45777938692f505641566d73794251517938617a736f7537787477410a333579355a754c365a3670585865576c774f7973434e73486d77575448347047586c6272656e797579324e6b435139392b7843440a2d2d2d2d2d454e44205253412050524956415445204b45592d2d2d2d2d0a">>,
      <<"a1effec1000000008219cc73f7a5be22c634bb81a363aebe75d1e22656e073b5b428564f2c0430ba32b85381a62ee5bca6f3fb8680efa0b2c6bdf3267dbddb74615713e382b5745c6f36361545631dd105ec0a346ef17b9bff3b41f9220e65f3e7919d36b0fc5345bb69d12de124e43b0105cddb662d371563dae2ed341f5dcef2c8df388cc1d53c18893100c4dd5accf6a9db69ed865068bb0f851b41b6c90b6d7b7613018b742c1c558e18478b80870066bac2477fe9d8a1ba51330c91f993f8eab116797fc3c8226c3a2a2d67d83d445d755cbfaac653b2489c1743b28f6c3431db10e9d5548900a4ef871c1049dcc12d693e12aa9d1de8057ca226ee94ebb4f16efe02bbae0b612f88bd3386fa821d9f5c8a59603ff5ce4381c66b8629d2d66e75c2803f97911b0e78e8b382ff27925b839b627bf02eeba1f1b9e56f902a32cd163d80d9535c50bff2014f8168d4b7a99bd6cab77dfd77d78301834f58ca2e28c43e42b98fd5329ecbfb55ef5b9f1ae27fca829a5ae5ccbed1491b4983243cd6710aeede6fc0371f9201bb85b2ca15d45db7157dc9a8cb323cd5386e0e8ff6413621848384dd3559370d4b32befb1182c2b68730f05a9e0e9ecf49bcb9bf3053142744b054b80567cf5fecf2b092663d6545a8a6725df89d15207d943a1c5254cdc6459314847aa567f599bac759208e5846920f8a191cdaed045f5ca8b138d17f16a125652dfb8848cf351fe29d7f4fd8143c0f1b010999b627a035bf642775b802375983b9b30aefdf786c08df8213f636dea70b7d0c08884a4bf94d19e16588f809c8f3fb02f730a67f1dc722b587856a6837fb08">>).
