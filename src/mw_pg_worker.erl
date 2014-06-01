%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% See https://github.com/devinus/poolboy
%%% PostgreSQL worker adapted from poolboy example. A set of these are launched
%%% as poolboy pool workers and keeps connection to postgres.
%%% @end
%%% Created : 01 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_pg_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("log.hrl").
-include("mw_pg.hrl").

-record(state, {conn = unset}).

%%%===========================================================================
%%% API
%%%===========================================================================
-spec start_link(any()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===========================================================================
%%% gen_server callbacks
%%%===========================================================================
init(Args) ->
    Host   = proplists:get_value(host, Args),
    Dbname = proplists:get_value(dbname, Args),
    User   = proplists:get_value(user, Args),
    Pass   = proplists:get_value(pass, Args),
    %% We will NOT crash during upstart if Postgres is unavailable, rather
    %% start the worker but have queries disabled.
    %% Damn epgsql crashing caller process if it can't start.
    erlang:process_flag(trap_exit, true),
    Conn =
        try
            case pgsql:connect(Host, User, Pass, [{database, Dbname}]) of
                {ok, C} ->
                    ?info("PostgreSQL worker connected to db ~p", [Dbname]),
                    C;
                {error, Error} ->
                    ?error("Could not connect to PostgreSQL. "
                           "Settings: ~p "
                           "Error: ~p ",[{Host, Dbname, User, Pass}, Error]),
                    no_conn
                end
        catch Error2:Reason ->
                Stack = erlang:get_stacktrace(),
                ?error("Could not connect to PostgreSQL. "
                       "Settings: ~p "
                       "Error: ~p ",[{Host, Dbname, User, Pass}, {Error2, Reason, Stack}]),
                no_conn
        end,
    erlang:process_flag(trap_exit, false),
    {ok, #state{conn = Conn}}.

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    Fun = fun(C) ->
                  pgsql:squery(C, Sql)
          end,
    Res = pgsql:with_transaction(Conn, Fun),
    {reply, Res, State};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    Fun = fun(C) ->
                  pgsql:equery(C, Stmt, Params)
          end,
    Res = pgsql:with_transaction(Conn, Fun),
    {reply, Res, State};
handle_call({transaction, Fun}, _, #state{conn=C}=State) ->
    Res = pgsql:with_transaction(C, Fun),
    {reply, Res, State};

handle_call(_Call, _From, State) ->
    ?warning("Unmatched call in ~p: ~p from ~p", [?MODULE, _Call, _From]),
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    ?warning("Unmatched cast in ~p: ~p", [?MODULE, _Cast]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?warning("Unmatched info in ~p: ~p", [?MODULE, _Info]),
    {noreply, State}.

terminate(_Reason, #state{conn=C}) ->
    ok = pgsql:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
