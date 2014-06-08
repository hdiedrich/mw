%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% PostgreSQL / epgsql query utility functions
%%% @end
%%% Created : 01 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_pg_lib).

-compile(export_all).
%% API
-export([]). %% TODO: remove export_all and add API exports

-include("log.hrl").

-define(QUERY_TIMEOUT,  5000).
-define(POOLNAME,       pgsql_pool).

%%%===========================================================================
%%% API
%%%===========================================================================
equery(Statement, Params) ->
    Equery = fun(Worker) ->
                     gen_server:call(Worker, {equery, Statement, Params},
                                     ?QUERY_TIMEOUT)
             end,
    case poolboy:transaction(?POOLNAME, Equery) of
        {error, Error} = E ->
            ?error("PGSQL equery failed. Query: ~p Error: ~p ",
                   [{Statement, Params}, Error]),
            E;
        Result ->
            Result
    end.

transaction(Fun) ->
    Tquery = fun(Worker) ->
                     gen_server:call(Worker, {transaction, Fun}, ?QUERY_TIMEOUT)
             end,
    case poolboy:transaction(?POOLNAME, Tquery) of
        {rollback, _} = E ->
            ?error("PGSQL transaction failed: ~p ", [E]),
            E;
        Result ->
            Result
    end.

ensure_epgsql_type(X) when is_float(X) -> X;
ensure_epgsql_type(X) when is_tuple(X) -> X;
ensure_epgsql_type(X) when is_binary(X) -> X;
ensure_epgsql_type(X) when is_integer(X) -> X;
ensure_epgsql_type(true) -> true;
ensure_epgsql_type(false) -> false;
ensure_epgsql_type(null) -> null;
ensure_epgsql_type(X) when is_atom(X) ->
    binary:list_to_bin(erlang:atom_to_list(X));
ensure_epgsql_type(X) when is_list(X) -> binary:list_to_bin(X);
ensure_epgsql_type(X) -> {error, {not_valid_epgsql_type, X}}.

parse_select_result({ok, _Columns, []}) ->
    {ok, []};
parse_select_result({ok, Columns, RowsTuples}) ->
    FormatColumn = fun({column, ColumnName, _SQLType,_,_,_}) -> ColumnName end,
    FormatRows = fun(RowsTuple) -> lists:zip(lists:map(FormatColumn, Columns),
                                             erlang:tuple_to_list(RowsTuple))
                 end,
    {ok, lists:map(FormatRows, RowsTuples)};
parse_select_result({error, Error}) ->
    {error, {sql_query_failed, Error}}.

parse_insert_result({ok, _Count}) -> {ok, sql_query_succeeded};
parse_insert_result({ok, _Count, Columns, [RowsTuple]}) ->
    FormatColumn = fun({column, ColumnName, _SQLType,_,_,_}) -> ColumnName end,
    {ok, lists:zip(lists:map(FormatColumn, Columns),
                   erlang:tuple_to_list(RowsTuple))};
parse_insert_result({error, Error}) ->
    {error, {sql_query_failed, Error}}.

parse_update_result({ok, Count})        -> {ok, Count};
parse_update_result({error, Error})    ->
    {error, Error}.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
