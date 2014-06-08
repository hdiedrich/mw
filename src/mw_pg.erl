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
select_contract(Id) ->
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
        "VALUES ( $1 );",
    {ok, _} =
        mw_pg_lib:parse_insert_result(
          mw_pg_lib:equery(Statement, [mw_pg_lib:ensure_epgsql_type(EventId)])),
    ok.

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

insert_oracle_keys(NoPubKey, NoPrivKey, YesPubKey, YesPrivKey) ->
    Statement =
        "INSERT INTO oracle_keys "
        "(rsa_no_pubkey, rsa_no_privkey, rsa_yes_pubkey, rsa_yes_privkey) "
        "VALUES ($1, $2, $3, $4);",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [NoPubKey, NoPrivKey, YesPubKey, YesPrivKey]),
    {ok, _} =
        mw_pg_lib:parse_insert_result(mw_pg_lib:equery(Statement, Params)),
    ok.

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
        "VALUES ( $1, $2, $3, $4, $5, $6, $7 );",
    Params = lists:map(fun mw_pg_lib:ensure_epgsql_type/1,
                       [MatchNum, Headline, Desc, OracleKeysId, EventPubKey,
                        EventPrivKeyEnvWithOracleNoKey,
                        EventPrivKeyEnvWithOracleYesKey]),
    {ok, _} = mw_pg_lib:parse_insert_result(mw_pg_lib:equery(Statement,
                                                             Params)),
    ok.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
