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
%%% Internal Erlang API (e.g. called by cron jobs / internal Mw services)
%%%===========================================================================
create_contract(EventId) ->
    ok = mw_pg:insert_contract(EventId),
    todo.

create_event() ->    
    todo.
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
manual_test_1() ->
    create_contract(1),
    ok.
