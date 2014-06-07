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

-include("log.hrl").
-include("mw_api_errors.hrl").

%%%===========================================================================
%%% Testnet keys for tests / dev / debug
%%%===========================================================================
-define(TEST_EVENT_PRIVKEY, <<"16C746A92F7584013A93004BE8A56709C8CFE0B71E8DEA6DEFC6BE0F7D9CB96C">>).
-define(TEST_EVENT_PUBKEY, <<"025A70A221894F315EBC864292D0DB9F7FF0F817C6D489D176E02BAA7FD4FCE320">>).

-define(TEST_ORACLE_NO_PRIVKEY, <<"7498560BFC3C501C6386A5EA936B548629E37A3E24C88C4FE29E969C83D8CC57">>).
-define(TEST_ORACLE_NO_PUBKEY, <<"039A7D381E13BA745BCA7BE5456E4076A63CA3B54FCB1B2CF7BD8ACA3DACBD06E3">>).

-define(TEST_ORACLE_YES_PRIVKEY, <<"CC74044B7931A452D7039DFE2C9985393843B724DB6251B54DC743E490E7C51C">>).
-define(TEST_ORACLE_YES_PUBKEY, <<"03DC052964F0BCA73CCA109B74CD8F7D2F82C4664AAC142D491DE8B4CC6D244492">>).

%%%===========================================================================
%%% JSON API handlers (called from cowboy callbacks)
%%%===========================================================================
%% Validations throw error so JSON handler can return nice error code / msg
%% Any unhandled error (crash) will return default json error code / msg
enter_contract(ContractId, PubKey) ->
    ?info("Handling enter_contract with ContractId: ~p , PubKey: ~p",
          [ContractId, PubKey]),
    api_validation(is_integer(ContractId), ?API_ERROR_CONTRACT_ID_TYPE),
    api_validation(is_binary(PubKey), ?API_ERROR_PUBKEY_TYPE),
    %% https://en.bitcoin.it/wiki/Technical_background_of_Bitcoin_addresses
    %% always 65 bytes == 130 hex digits
    api_validation((byte_size(PubKey) == 130), ?API_ERROR_PUBKEY_LEN),

    %% ok = do_enter_contract(ContractId, PubKey),
    [{"success-message", "ok_todo"}].

%%%===========================================================================
%%% Internal Erlang API (e.g. called by cron jobs / internal Mw services) but
%%% which may be exposed as JSON API later on
%%%===========================================================================
create_contract(EventId) ->
    ok = mw_pg:insert_contract(EventId),
    ok.

create_event(MatchNum, Headline, Desc, OracleKeysId, EventPubKey) ->
    {ok, OracleNOPubKey, OracleYESPubKey} = mw_pg:select_oracle_keys(OracleKeysId),
    ok.

create_oracle_keys(NOPubKey, YESPubKey) ->
    api_validation(is_binary(NOPubKey), ?API_ERROR_PUBKEY_TYPE),
    api_validation(is_binary(YESPubKey), ?API_ERROR_PUBKEY_TYPE),
    api_validation((byte_size(NOPubKey) == 130), ?API_ERROR_PUBKEY_LEN),
    api_validation((byte_size(YESPubKey) == 130), ?API_ERROR_PUBKEY_LEN),
    ok = mw_pg:insert_oracle_keys(NOPubKey, YESPubKey),
    ok.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
do_enter_contract(ContractId, PubKey) ->
    [{"todo", "todo_stuff"}].


api_validation(false, {ErrorCode, ErrorMsg}) ->
    throw({api_error, {ErrorCode, ErrorMsg}});
api_validation(true, _) ->
    continue.

%%%===========================================================================
%%% Dev / Debug / Manual Tests
%%%===========================================================================
%% mw_contract:manual_test_1().
manual_test_1() ->
    ok = create_oracle_keys(?TEST_ORACLE_NO_PUBKEY, ?TEST_ORACLE_YES_PUBKEY),
    ok = create_event(1, "Brazil beats Croatia", "More foo info", 1, ?TEST_EVENT_PUBKEY),
    %% create_contract(1),
    ok.
