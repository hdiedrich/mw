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

-define(BJ_MOCKED, true).
-define(BJ_URL_GET_UNSIGNED_T2,     <<"http://localhost/get-unsigned-t2">>).
-define(BJ_URL_GET_UNSIGNED_T3,     <<"http://localhost/get-unsigned-t3">>).
-define(BJ_URL_SUBMIT_T2_SIGNATURE, <<"http://localhost/submit-t2-signature">>).
-define(BJ_URL_SUBMIT_T3_SIGNATURES, <<"http://localhost/submit-t3-signatures">>).
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
enter_contract(ContractId, ECPubKey, RSAPubKey) ->
    ?info("Handling enter_contract with ContractId: ~p , ECPubKey: ~p"
          "RSAPubKey: ~p", [ContractId, ECPubKey, RSAPubKey]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),

    %% https://en.bitcoin.it/wiki/Base58Check_encoding
    %% compressed EC pubkeys in base58check encoding is 50 chars
    api_validation(is_binary(ECPubKey) andalso
                   is_binary(catch mw_lib:dec_b58check(ECPubKey)),
                   ?EC_PUBKEY_TYPE),
    api_validation((byte_size(ECPubKey) == 50), ?EC_PUBKEY_LEN),

    api_validation(is_binary(RSAPubKey) andalso
                   %% http://erlang.org/doc/man/public_key.html#pem_decode-1
                   length(catch public_key:pem_decode(RSAPubKey)) == 1,
                   ?RSA_PUBKEY_TYPE),
    %% TODO: what lengths can PEM encoded RSA 2048 pubkeys have?
    % api_validation((byte_size(RSAPubKey) == 902), ?RSA_PUBKEY_LEN),

    ok = do_enter_contract(ContractId, ECPubKey, RSAPubKey),
    [{"success-message", "ok"}].

submit_t2_signature(ContractId, ECPubKey, T2Signature) ->
    ?info("Handling submit_signed_t2_hash with ContractId: ~p , ECPubKey: ~p, "
          "T2Signature: ~p",
          [ContractId, ECPubKey, T2Signature]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),

    api_validation(is_binary(ECPubKey) andalso
                   is_binary(catch mw_lib:dec_b58check(ECPubKey)),
                   ?EC_PUBKEY_TYPE),
    api_validation((byte_size(ECPubKey) == 50), ?EC_PUBKEY_LEN),

    api_validation(is_binary(catch mw_lib:hex_to_bin(T2Signature)),
                   ?SIGNATURE_TYPE),
    api_validation(bitcoin_signature_der(mw_lib:hex_to_bin(T2Signature)),
                   ?SIGNATURE_TYPE),

    ok = do_submit_t2_signature(ContractId, ECPubKey, T2Signature),
    [{"success-message", "ok"}].

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

    ResultJSON = do_get_t3_for_signing(ContractId, ToAddress),
    [{"success-message", "ok"}] ++ ResultJSON.

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
    [{"success-message", "ok"}].

%%%===========================================================================
%%% Internal Erlang API (e.g. called by cron jobs / internal Mw services) but
%%% which may be exposed as JSON API later on
%%%===========================================================================
%% For MVP #2 this can be used for pages: prep, pend, sign and status
get_contract_t2_state(Id) ->
    {ok, Info}  = get_contract_info(Id),
    GetInfo     = ?GET(Info),
    History     = GetInfo("history"),
    EventPubKey = GetInfo("event_pubkey"),
    GiverPubKey = GetInfo("giver_ec_pubkey"),
    TakerPubKey = GetInfo("taker_ec_pubkey"),
    Value       = <<"2000000">>,

    %% TODO: for now we simplify flow and assume both have sent T1
    %% when we get first T2 from Bj
    case {contract_event_happened(History, ?STATE_DESC_GIVER_ENTERED),
          contract_event_happened(History, ?STATE_DESC_TAKER_ENTERED),
          contract_event_happened(History, ?STATE_DESC_GIVER_T1),
          contract_event_happened(History, ?STATE_DESC_TAKER_T1)} of
        {true, true, false, false} ->
            %% call Bj to see if t1 outputs are available as t2 inputs
            ReqRes = bj_req_get_unsigned_t2(GiverPubKey, TakerPubKey,
                                            EventPubKey, Value),
            ?info("bj_req_get_unsigned_t2: ~p", [ReqRes]),
            GetRes = ?GET(ReqRes),
            case GetRes("error-message") of
                not_found ->
                    T2SigHashInput0 = GetRes("t2-sighash-input-0"), %% giver
                    T2SigHashInput1 = GetRes("t2-sighash-input-1"), %% taker
                    T2Raw = GetRes("t2-raw"), %% taker
                    T2Hash = GetRes("t2-hash"),
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
     EventPubKey, GiverECPubKey, TakerECPubKey,
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
          {"event_pubkey", EventPubKey},
          {"giver_ec_pubkey", GiverECPubKey},
          {"taker_ec_pubkey", TakerECPubKey},
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

create_oracle_keys(NoPubKey, NoPrivKey, YesPubKey, YesPrivKey) ->
    %% Validations for EC keys
    %%api_validation(is_binary(NOPubKey), ?EC_PUBKEY_TYPE),
    %%api_validation(is_binary(YESPubKey), ?EC_PUBKEY_TYPE),
    %%api_validation((byte_size(NOPubKey) == 130), ?PUBKEY_LEN),
    %%api_validation((byte_size(YESPubKey) == 130), ?PUBKEY_LEN),
    {ok, Id} = mw_pg:insert_oracle_keys(NoPubKey, NoPrivKey,
                                        YesPubKey, YesPrivKey),
    {ok, Id}.

create_event(MatchNum, Headline, Desc, OracleKeysId,
             EventPrivKey, EventPubKey) ->
    {ok, NoPubKeyPEM, YesPubKeyPEM} = mw_pg:select_oracle_keys(OracleKeysId),
    {ok, NoPubKey}  = pem_decode_bin(NoPubKeyPEM),
    {ok, YesPubKey} = pem_decode_bin(YesPubKeyPEM),
    EventPrivKeyEncWithOracleNoKey =
        hybrid_aes_rsa_enc(EventPrivKey, NoPubKey),
    EventPrivKeyEncWithOracleYesKey =
        hybrid_aes_rsa_enc(EventPrivKey, YesPubKey),
    {ok, EventId} =
        mw_pg:insert_event(MatchNum, Headline, Desc, OracleKeysId, EventPubKey,
                           EventPrivKeyEncWithOracleNoKey,
                           EventPrivKeyEncWithOracleYesKey),
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
do_enter_contract(ContractId, ECPubKey, RSAPubKeyHex) ->
    {ok, RSAPubKey} = pem_decode_bin(RSAPubKeyHex),
    {YesOrNo, GiverOrTaker, _GiverKey} =
        case mw_pg:select_contract_ec_pubkeys(ContractId) of
            {ok, null, null}          -> {yes, giver, nope};
            {ok, GiverECPubKey, null} -> {no, taker, GiverECPubKey};
            {ok, _GiverECPubKey, _TakerECPubKey} ->
                ?API_ERROR(?CONTRACT_FULL);
            {error,{ok,[]}} ->
                ?API_ERROR(?CONTRACT_NOT_FOUND)
        end,
    {ok, EncEventKey} =
        mw_pg:select_enc_event_privkey(ContractId, YesOrNo),
    DoubleEncEventKey = hybrid_aes_rsa_enc(EncEventKey, RSAPubKey),
    ok = mw_pg:update_contract_enter(ContractId, GiverOrTaker,
                               ECPubKey, RSAPubKeyHex, DoubleEncEventKey),
    EnteredEvent = case GiverOrTaker of
                       giver -> ?STATE_DESC_GIVER_ENTERED;
                       taker -> ?STATE_DESC_TAKER_ENTERED
                   end,
    ok = mw_pg:insert_contract_event(ContractId, EnteredEvent),
    ok.

do_submit_t2_signature(ContractId, ECPubKey, T2Signature) ->
    {ok, Info}  = get_contract_info(ContractId),
    GetInfo     = ?GET(Info),
    GiverPubKey = GetInfo("giver_ec_pubkey"),
    TakerPubKey = GetInfo("taker_ec_pubkey"),

    %% validate contract event states? e.g. duplicated signing reqs
    GiverOrTaker = case ECPubKey of
                       GiverPubKey -> <<"giver">>;
                       TakerPubKey -> <<"taker">>;
                       _           -> ?API_ERROR(?EC_PUBKEY_MISMATCH)
                   end,
    T2Raw = GetInfo("t2_raw"),
    ReqRes =
        bj_req_submit_t2_signature(ECPubKey, T2Signature, T2Raw, GiverOrTaker),
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
             {"oracle_privkey", OPK},
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
        "true" -> awesome;
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

bj_req_get_unsigned_t2(GiverPubKey, TakerPubKey, EventPubKey,
                       Value) ->
    QS = cow_qs:qs(
           [
            {<<"giver-pubkey">>, GiverPubKey},
            {<<"taker-pubkey">>, TakerPubKey},
            {<<"event-pubkey">>, EventPubKey},
            {<<"value">>, Value}
           ]),
    %% TODO: parse response to proplist
    {ok, Res} =
        case ?BJ_MOCKED of
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
                mw_lib:bj_http_req(<<?BJ_URL_GET_UNSIGNED_T2/binary,
                                     $?, QS/binary>>, [], 5000)
        end,
    Res.

bj_req_submit_t2_signature(ECPubKey, T2Signature, T2Raw, GiverOrTaker) ->
    QS = cow_qs:qs(
           [
            {<<"t2-signature">>, T2Signature},
            {<<"t2-raw">>, T2Raw},
            {<<"pubkey">>, ECPubKey},
            {<<"sign-for">>, GiverOrTaker}
           ]),
    %% TODO: parse response to proplist
    {ok, Res} =
        case ?BJ_MOCKED of
            true ->
                {ok,
                 [
                  {"new-t2-hash", "A1EFFEC100000000FF04"},
                  {"t2-raw-partially-signed", "A1EFFEC100000000FF05"},
                  {"t2-broadcasted", "true"}
                 ]
                 };
            false ->
                mw_lib:bj_http_req(<<?BJ_URL_SUBMIT_T2_SIGNATURE/binary,
                                     $?, QS/binary>>, [], 5000)
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
        case ?BJ_MOCKED of
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
        case ?BJ_MOCKED of
            true ->
                {ok,
                 [
                  {"new-t3-hash", "A1EFFEC100000000FF09"},
                  {"new-t3-raw", "A1EFFEC100000000FF10"},
                  {"t3-broadcasted", "true"}
                 ]
                 };
            false ->
                mw_lib:bj_http_req(<<?BJ_URL_SUBMIT_T3_SIGNATURES /binary,
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

hybrid_aes_rsa_enc(Plaintext, RSAPubKey) ->
    %% TODO: validate entropy source! We may want to add extra entropy to be
    %% absolutely sure the AES key is cryptographically strong
    AESKey = crypto:strong_rand_bytes(16),
    %% PKCS #7 padding; value of each padding byte is the integer representation
    %% of the number of padding bytes. We align to 16 bytes.
    PaddingLen = 16 - (byte_size(Plaintext) rem 16),
    Padding = binary:copy(<<PaddingLen>>, PaddingLen),
    PaddedPlaintext = <<Plaintext/binary, Padding/binary>>,
    Ciphertext = crypto:block_encrypt(aes_cbc128, AESKey,
                                      ?DEFAULT_AES_IV, PaddedPlaintext),
    EncAESKey = public_key:encrypt_public(AESKey, RSAPubKey),
    %% Distinguishable prefix to identify the binary in case it's on the loose
    <<(mw_lib:hex_to_bin(?BINARY_PREFIX)):8/binary,
      EncAESKey:128/bytes,
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

decryption_test() ->
    ok.
