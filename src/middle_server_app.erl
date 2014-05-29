%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.1.x/initial spike                                       %%%
%%% File        : middle_server_app.erl                                     %%%
%%% Description : main module, starting the Cowboy host                     %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 24 May 2014                                               %%%
%%% Changed     : 27 May 2014                                               %%%
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
    %% TODO: application:ensure_all_started
    application:start(lager),
    %% define hosts, pathes, their patterns and handlers
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/index.html", page_handler,
                          {}},
                         {"/[...]", cowboy_static,
                          {priv_dir, middle_server, "",
                           [{mimetypes, cow_mimetypes, all}]}
                         }
                        ]}
                 ]),

    %% start cowboy http server
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [
                                 {env, [{dispatch, Dispatch}]},
                                 {middlewares, [cowboy_router, cowboy_handler]}
                                ]),

    %% start request-handling middleware
    middle_server_sup:start_link().

stop(_State) ->
    ok.
