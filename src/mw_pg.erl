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
        "WHERE c.id = $1",

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
%%%===========================================================================
%%% Internal functions
%%%===========================================================================
