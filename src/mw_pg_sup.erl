%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% See https://github.com/devinus/poolboy
%%% Exclusive supervisor for pgsql pool workers
%%% @end
%%% Created : 01 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_pg_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%============================================================================
%%% API functions
%%%============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%============================================================================
%%% Supervisor callbacks
%%%============================================================================
init([]) ->
    {ok, Pools} = application:get_env(mw, pools),
    PoolSpecs = lists:map(fun build_pool_spec/1, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
build_pool_spec({Name, SizeArgs, WorkerArgs}) ->
    PoolArgs = [{name, {local, Name}},
                {worker_module, mw_pg_worker}] ++ SizeArgs,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs).
