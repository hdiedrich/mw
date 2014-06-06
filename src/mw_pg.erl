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
get_contract(ContractId) ->
    Statement =
        "SELECT c.id"
        "FROM contracts c "
        "WHERE c.id = $1;",

    Res =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement,
                              [mw_pg_lib:ensure_epgsql_type(ContractId)])),
    case Res of
        {ok, [[{<<"id">>, Id}]]} -> {ok, Id};
        Other                    -> {error, Other}
    end.

insert_contract(EventId) ->
    Statement =
        "INSERT INTO contracts (event_id) "
        "VALUES ( $1 );",
    {ok, _} =
        mw_pg_lib:parse_insert_result(
          mw_pg_lib:equery(Statement,
                           [mw_pg_lib:ensure_epgsql_type(EventId)])),
    ok.

select_oracle_keys(Id) ->
    Statement =
        "SELECT ok.rsa_no_pubkey, ok.rsa_yes_pubkey "
        "FROM oracle_keys ok "
        "WHERE ok.id = $1;",
    Res =
        mw_pg_lib:parse_select_result(
          mw_pg_lib:equery(Statement,
                           [mw_pg_lib:ensure_epgsql_type(Id)])),
    case Res of
        {ok, [[{<<"rsa_no_pubkey">>, NoPubKey},
               {<<"rsa_yes_pubkey">>, YesPubKey}]]} ->
            {ok, NoPubKey, YesPubKey};
        Other -> {error, Other}
    end.

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
    Params = [mw_pg_lib:ensure_epgsql_type(E) ||
                 E <- [MatchNum, Headline, Desc, OracleKeysId, EventPubKey,
                       EventPrivKeyEnvWithOracleNoKey,
                       EventPrivKeyEnvWithOracleYesKey]],
    {ok, _} = mw_pg_lib:parse_insert_result(mw_pg_lib:equery(Statement,
                                                             Params)),
    ok.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
