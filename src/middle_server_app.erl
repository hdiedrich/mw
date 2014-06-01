%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.1.x/initial spike                                       %%%
%%% File        : middle_server_app.erl                                     %%%
%%% Description : main module, starting the Cowboy host                     %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 24 May 2014                                               %%%
%%% Changed     : 29 May 2014                                               %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%  The AIX WC14 concept enables football bets on the Bitcoin blockchain   %%%
%%%  that are decentralized, oracle-driven contacts, requiring less trust.  %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%

-module(middle_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
        %% TODO: extract to config file
        application:load(lager),
        application:set_env(lager, error_logger_hwm, 500),
        %% TODO: application:ensure_all_started
        application:start(lager),

        %% -------------------------------------------------------------------
        %% API
        %% -------------------------------------------------------------------

        %% define json hosts, pathes, their patterns and handlers
        JSONDispatch = cowboy_router:compile([
                {'_', [{"/hello", api_handler, hello},
                       {"/sample", api_handler, sample},
                       {"/bet-list", api_handler, 'bet-list'}]}
        ]),

        %% start cowboy json server
        {ok, _} = cowboy:start_http(json, 100, [{port, 8081}], [
                {env, [{dispatch, JSONDispatch}]},
                {middlewares, [cowboy_router, cowboy_handler]}
        ]),

        %% -------------------------------------------------------------------
        %% Web Site
        %% -------------------------------------------------------------------

        %% define http hosts, pathes, their patterns and handlers
        HTMLDispatch = cowboy_router:compile([
                {'_', [
                        {"/index.html", page_handler, {}},
                        {"/[...]", cowboy_static,
                                {priv_dir, middle_server, "",
                                         [{mimetypes, cow_mimetypes, all}]}}
                ]}
        ]),

        %% start cowboy http server
        {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
                {env, [{dispatch, HTMLDispatch}]},
                {middlewares, [cowboy_router, cowboy_handler]}
        ]),

        %% start request-handling middleware
        middle_server_sup:start_link().

stop(_State) ->
        ok.
