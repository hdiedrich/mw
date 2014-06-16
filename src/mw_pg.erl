%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% PostgreSQL / epgsql query utility functions
%%% @end
%%% Created : 01 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_pg).

-compile(export_all).
%% API
-export([]). %% TODO: remove export_all and add API exports

-include("log.hrl").

%%%===========================================================================
%%% API
%%%===========================================================================
%% TODO: add timestamp as parameter, as e.g. events like T2 mined/broadcast
%% is not exactly time when we insert event
insert_contract_event(ContractId, Event) ->
    Statement =
        "INSERT INTO contract_events (time, description) "
        "VALUES "
        "( CAST(now() AT TIME ZONE 'UTC' AS timestamp with time zone), $1 ) "
        "RETURNING id;",
    {ok, [{<<"id">>, ContractEventId}]} =
        mw_pg_lib:parse_insert_result(
          mw_pg_lib:equery(Statement, [
                                       mw_pg_lib:ensure_epgsql_type(Event)
                                      ])),
    Statement2 =
        "INSERT INTO contract_events_maps (contract_id, contract_event_id) "
        "VALUES ( $1, $2 );",
    {ok, _} =
        mw_pg_lib:parse_insert_result(
          mw_pg_lib:equery(Statement2,
                           [
                            mw_pg_lib:ensure_epgsql_type(ContractId),
                            mw_pg_lib:ensure_epgsql_type(ContractEventId)
                           ])),
    ok.

select_contract_infos() ->
    Statement =
        "SELECT e.id as event_id, e.match_no, e.headline, e.description, e.outcome, "
        "       e.event_pubkey, c.giver_ec_pubkey, c.taker_ec_pubkey, "
        "       c.t2_sighash_input_0, c.t2_sighash_input_1, t2_raw "
        "FROM events e
         JOIN contracts c ON e.id = c.event_id;",
        mw_pg_lib:parse_select_result(
                mw_pg_lib:equery(Statement, [])).


select_contract_info(Id) ->
    Statement =
        "SELECT ce.time AT TIME ZONE 'UTC', ce.description "
        "FROM contracts c, contract_events ce, contract_events_maps cem "
        "WHERE ce.id = cem.contract_event_id and cem.contract_id = c.id and "
        "c.id = $1;",
    {ok, Events} =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement,
                           [mw_pg_lib:ensure_epgsql_type(Id)])),
    Statement2 =
        "SELECT e.match_no, e.headline, e.description, e.outcome, "
        "       e.event_pubkey, c.giver_ec_pubkey, c.taker_ec_pubkey, "
        "       c.t2_sighash_input_0, c.t2_sighash_input_1, t2_raw "
        "FROM events e, contracts c "
        "WHERE e.id = c.event_id and c.id = $1;",
    {ok, [[{<<"match_no">>, MatchNo},
           {<<"headline">>, Headline},
           {<<"description">>, Desc},
           {<<"outcome">>, Outcome},
           {<<"event_pubkey">>, EventPubKey},
           {<<"giver_ec_pubkey">>, GiverECPubKey},
           {<<"taker_ec_pubkey">>, TakerECPubKey},
           {<<"t2_sighash_input_0">>, T2SigHashInput0},
           {<<"t2_sighash_input_1">>, T2SigHashInput1},
           {<<"t2_raw">>, T2Raw}
          ]]} =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement2,
                           [mw_pg_lib:ensure_epgsql_type(Id)])),

    FormatEvent =
        fun([{<<"timezone">>, DT},
             {<<"description">>, D}]) ->
            {mw_lib:datetime_to_iso_timestamp(DT), D}
        end,
    FormatedEvents = lists:map(FormatEvent, Events),
    {ok, MatchNo, Headline, Desc, Outcome,
     EventPubKey, GiverECPubKey, TakerECPubKey,
     T2SigHashInput0, T2SigHashInput1, T2Raw,
     FormatedEvents}.

select_contract_ec_pubkeys(Id) ->
    Statement =
        "SELECT c.giver_ec_pubkey, c.taker_ec_pubkey "
        "FROM contracts c "
        "WHERE c.id = $1;",

    Res =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement,
                           [mw_pg_lib:ensure_epgsql_type(Id)])),
    case Res of
        {ok, [[{<<"giver_ec_pubkey">>, GiverECPubKey},
               {<<"taker_ec_pubkey">>, TakerECPubKey}]]} ->
            {ok, GiverECPubKey, TakerECPubKey};
        Other ->
            {error, Other}
    end.

insert_contract(EventId) ->
    Statement =
        "INSERT INTO contracts (event_id) "
        "VALUES ( $1 ) RETURNING id;",
    {ok, [{<<"id">>, NewId}]} =
        mw_pg_lib:parse_insert_result(
          mw_pg_lib:equery(Statement, [mw_pg_lib:ensure_epgsql_type(EventId)])),
    {ok, NewId}.

clone_contract(Id) ->
    Statement =
        "SELECT c.event_id "
        "FROM contracts c "
        "WHERE c.id = $1;",

    {ok, [[{<<"event_id">>, EventId}]]} =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement, [mw_pg_lib:ensure_epgsql_type(Id)])),
    S = "INSERT INTO contracts (event_id) VALUES ( $1 ) RETURNING id;",
    {ok, [{<<"id">>, NewId}]} =
        mw_pg_lib:parse_insert_result(mw_pg_lib:equery(S, [EventId])),
    {ok, NewId}.

%% Add giver or taker
update_contract(Id, GiverOrTaker, ECPubKey, RSAPubKey, EventKeyDoubleEnc) ->
    {ECKeyTable, RSAKeyTable, EventKeyTable} =
        case GiverOrTaker of
            giver -> {"giver_ec_pubkey", "giver_rsa_pubkey",
                      "event_key_enc_with_oracle_yes_and_giver_keys"};
            taker -> {"taker_ec_pubkey", "taker_rsa_pubkey",
                      "event_key_enc_with_oracle_no_and_taker_keys"}
        end,
    Statement =
        "UPDATE contracts SET " ++
        ECKeyTable    ++ " = " ++ "$1, " ++
        RSAKeyTable   ++ " = " ++ "$2, " ++
        EventKeyTable ++ " = " ++ "$3 "  ++
        "WHERE id = $4;",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [ECPubKey, RSAPubKey, EventKeyDoubleEnc, Id]),
    {ok, _} = mw_pg_lib:parse_insert_result(
                mw_pg_lib:equery(Statement, Params)),
    ok.

%% Add t2 sighashes and t2 raw
update_contract(Id, T2SigHashInput0, T2SigHashInput1, T2Raw) ->
    Statement =
        "UPDATE contracts SET "
        "t2_sighash_input_0 = $1, "
        "t2_sighash_input_1 = $2, "
        "t2_raw = $3 "
        "WHERE id = $4;",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [T2SigHashInput0, T2SigHashInput1, T2Raw, Id]),
    {ok, _} = mw_pg_lib:parse_insert_result(
                mw_pg_lib:equery(Statement, Params)),
    ok.

%% Update t2 raw after adding one signature to it
update_contract(Id, T2Raw) ->
    Statement =
        "UPDATE contracts SET "
        "t2_raw = $1 "
        "WHERE id = $2;",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [T2Raw, Id]),
    {ok, _} = mw_pg_lib:parse_insert_result(
                mw_pg_lib:equery(Statement, Params)),
    ok.

insert_oracle_keys(NoPubKey, NoPrivKey, YesPubKey, YesPrivKey) ->
    Statement =
        "INSERT INTO oracle_keys "
        "(rsa_no_pubkey, rsa_no_privkey, rsa_yes_pubkey, rsa_yes_privkey) "
        "VALUES ($1, $2, $3, $4) "
        "RETURNING id;",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [NoPubKey, NoPrivKey, YesPubKey, YesPrivKey]),
    {ok, [{<<"id">>, NewId}]} =
        mw_pg_lib:parse_insert_result(mw_pg_lib:equery(Statement, Params)),
    {ok, NewId}.

select_oracle_keys(_Id) ->
    Statement =
        "SELECT ok.rsa_no_pubkey, ok.rsa_yes_pubkey "
        "FROM oracle_keys ok "
        "LIMIT 1;",
    %% TODO: change to support multiple oracles
    %% "WHERE ok.id = $1;",
    {ok, [[{<<"rsa_no_pubkey">>, NoPubKey},
           {<<"rsa_yes_pubkey">>, YesPubKey}]]} =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement,
                           [
                            %% mw_pg_lib:ensure_epgsql_type(Id)
                           ])),
    {ok, NoPubKey, YesPubKey}.

select_enc_event_privkey(ContractId, YesOrNo) ->
    TabName = case YesOrNo of
                  no -> <<"event_privkey_enc_with_oracle_no_pubkey">>;
                  yes -> <<"event_privkey_enc_with_oracle_yes_pubkey">>
              end,
    Statement =
        "SELECT e." ++ binary:bin_to_list(TabName) ++ " "
        "FROM events e, contracts c "
        "where e.id = c.event_id AND c.id = $1;",
    {ok, [[{TabName, EncPrivKey}]]} =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement,
                           [
                            mw_pg_lib:ensure_epgsql_type(ContractId)
                           ])),
    {ok, EncPrivKey}.

%% New event; some fields are not available yet
insert_event(MatchNum, Headline, Desc, OracleKeysId, EventPubKey,
             EventPrivKeyEnvWithOracleNoKey, EventPrivKeyEnvWithOracleYesKey) ->
    Statement =
        "INSERT INTO events "
        "(match_no, headline, description, "
        "oracle_keys_id, event_pubkey, "
        "event_privkey_enc_with_oracle_no_pubkey, "
        "event_privkey_enc_with_oracle_yes_pubkey) "
        "VALUES ( $1, $2, $3, $4, $5, $6, $7 ) "
        "RETURNING id;",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [MatchNum, Headline, Desc, OracleKeysId, EventPubKey,
                        EventPrivKeyEnvWithOracleNoKey,
                        EventPrivKeyEnvWithOracleYesKey]),
    {ok, [{<<"id">>, EventId}]} =
        mw_pg_lib:parse_insert_result(mw_pg_lib:equery(Statement,
                                                       Params)),
    {ok, EventId}.
