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

-define(DEFAULT_AES_IV, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).
-define(BINARY_PREFIX, <<"A1EFFEC100000000">>).

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
    ?info("Handling enter_contract with ContractId: ~p , ECPubKey: ~p",
          [ContractId, ECPubKey]),
    api_validation(is_integer(ContractId), ?CONTRACT_ID_TYPE),
    api_validation(is_binary(ECPubKey), ?PUBKEY_TYPE),
    %% https://en.bitcoin.it/wiki/Technical_background_of_Bitcoin_addresses
    %% always 65 bytes == 130 hex digits
    api_validation((byte_size(ECPubKey) == 130), ?EC_PUBKEY_LEN),

    api_validation(is_binary(RSAPubKey), ?PUBKEY_TYPE),
    %?info("rsa pubkey len ~p", [byte_size(RSAPubKey)]),
    api_validation((byte_size(RSAPubKey) == 902), ?RSA_PUBKEY_LEN),

    ok = do_enter_contract(ContractId, ECPubKey, RSAPubKey),
    [{"success-message", "ok"}].

%%%===========================================================================
%%% Internal Erlang API (e.g. called by cron jobs / internal Mw services) but
%%% which may be exposed as JSON API later on
%%%===========================================================================
create_contract(EventId) ->
    {ok, ContractId} = mw_pg:insert_contract(EventId),
    ok = mw_pg:insert_contract_event(ContractId, ?CONTRACT_STATE_DESC_CREATED),
    ok.

clone_contract(Id) ->
    {ok, NewId} = mw_pg:clone_contract(Id),
    ok = mw_pg:insert_contract_event(NewId, ?CONTRACT_STATE_DESC_CLONED),
    [{"new_contract", NewId}].

create_oracle_keys(NoPubKey, NoPrivKey, YesPubKey, YesPrivKey) ->
    %% Validations for EC keys
    %api_validation(is_binary(NOPubKey), ?PUBKEY_TYPE),
    %api_validation(is_binary(YESPubKey), ?PUBKEY_TYPE),
    %api_validation((byte_size(NOPubKey) == 130), ?PUBKEY_LEN),
    %api_validation((byte_size(YESPubKey) == 130), ?PUBKEY_LEN),
    ok = mw_pg:insert_oracle_keys(NoPubKey, NoPrivKey, YesPubKey, YesPrivKey),
    ok.

create_event(_MatchNum, Headline, Desc, OracleKeysId, EventPrivKey, EventPubKey) ->
    {ok, NoPubKeyPEM, YesPubKeyPEM} = mw_pg:select_oracle_keys(OracleKeysId),
    {ok, NoPubKey}  = pem_decode_bin(NoPubKeyPEM),
    {ok, YesPubKey} = pem_decode_bin(YesPubKeyPEM),
    EventPrivKeyEncWithOracleNoKey =
        hybrid_aes_rsa_enc(EventPrivKey, NoPubKey),
    EventPrivKeyEncWithOracleYesKey =
        hybrid_aes_rsa_enc(EventPrivKey, YesPubKey),
    ok = mw_pg:insert_event(1, Headline, Desc, OracleKeysId, EventPubKey,
                            EventPrivKeyEncWithOracleNoKey,
                            EventPrivKeyEncWithOracleYesKey),
    ok.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
get_contract_info(Id) ->
    {ok, MatchNo, Headline, Desc, Outcome, FormatedEvents} =
        mw_pg:select_contract_info(Id),
    {ok, [
          {"match_no", MatchNo},
          {"headline", Headline},
          {"desc", Desc},
          {"outcome", Outcome},
          {"history", lists:map(fun({Timestamp, Event}) ->
                                        [{"timestamp", Timestamp},
                                         {"event", Event}]
                                end, FormatedEvents)}
         ]}.

create_contract_event(Event) ->
    ok = mw_pg:insert_contract_event(Event),
    ok.

%% TODO: think about abstraction concerns regarding matching on postgres 'null'
%% TODO: this assumes giver always enters first
%% TODO: generalize
do_enter_contract(ContractId, ECPubKey, RSAPubKeyHex) ->
    {ok, RSAPubKey} = pem_decode_bin(mw_lib:hex_to_bin(RSAPubKeyHex)),
    {YesOrNo, GiverOrTaker, _GiverKey} =
        case mw_pg:select_contract_ec_pubkeys(ContractId) of
            {ok, null, null}          -> {yes, giver, nope};
            {ok, GiverECPubKey, null} -> {no, taker, GiverECPubKey};
            {ok, _GiverECPubKey, _TakerECPubKey} -> ?API_ERROR(?CONTRACT_FULL);
            {error,{ok,[]}}                      -> ?API_ERROR(?CONTRACT_NOT_FOUND)
        end,
    {ok, EncEventKey} =
        mw_pg:select_enc_event_privkey(ContractId, YesOrNo),
    DoubleEncEventKey = hybrid_aes_rsa_enc(EncEventKey, RSAPubKey),
    ok = mw_pg:update_contract(ContractId, GiverOrTaker,
                               ECPubKey, RSAPubKeyHex, DoubleEncEventKey),
    EnteredEvent = case GiverOrTaker of
                       giver -> ?CONTRACT_STATE_DESC_GIVER_ENTERED;
                       taker -> ?CONTRACT_STATE_DESC_TAKER_ENTERED
                   end,
    ok = mw_pg:insert_contract_event(ContractId, EnteredEvent),
    ok.

api_validation(false, APIError) -> ?API_ERROR(APIError);
api_validation(true, _) -> continue.

pem_decode_bin(Bin) ->
    [Entry] = public_key:pem_decode(Bin),
    Key = public_key:pem_entry_decode(Entry),
    {ok, Key}.

rsa_key_from_file(PrivPath) ->
    AbsPath = filename:join(code:priv_dir(middle_server), PrivPath),
    {ok, Bin} = file:read_file(AbsPath),
    %{ok, Key} = pem_decode_bin(Bin),
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
    ok = create_oracle_keys(rsa_key_from_file("test_keys/oracle_no_pubkey.pem"),
                            rsa_key_from_file("test_keys/oracle_no_privkey.pem"),
                            rsa_key_from_file("test_keys/oracle_yes_pubkey.pem"),
                            rsa_key_from_file("test_keys/oracle_yes_privkey.pem")),
    ok = create_event(1, "Brazil beats Croatia", "More foo info", 1,
                      mw_lib:hex_to_bin(?TEST_EC_EVENT_PRIVKEY),
                      mw_lib:hex_to_bin(?TEST_EC_EVENT_PUBKEY)),
    ok.

manual_test_2() ->
    create_contract(1),
    ok.
