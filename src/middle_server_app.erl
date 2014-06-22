%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.5.x/first round trip                                    %%%
%%% File        : middle_server_app.erl                                     %%%
%%% Description : main module, starting the Cowboy host                     %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 24 May 2014                                               %%%
%%% Changed     : 12 June 2014                                              %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%  The AIX WC14 concept enables football bets on the Bitcoin blockchain   %%%
%%%  that are decentralized, oracle-driven contracts, requiring less trust. %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%

-module(middle_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("log.hrl").

start(_Type, _Args) ->
    %% TODO: extract to node config file
    application:load(lager),
    application:load(mw),
    application:set_env(mw, pools,
                        [
                         {pgsql_pool, [{size, 1}, {max_overflow, 1}],
                          [
                           {host, "localhost"},
                           {dbname, "mw"},
                           {user, "mw"},
                           {pass, "mw"}
                          ]}
                        ]),

    %% TODO: application:ensure_all_started
    application:start(lager),
    application:set_env(lager, error_logger_hwm, 500),
    application:start(jiffy),

    %% -------------------------------------------------------------------
    %% API
    %% -------------------------------------------------------------------
    %% define json hosts, pathes, their patterns and handlers
    JSONDispatch =
        cowboy_router:compile(
          [
           {'_',
            [{"/hello", api_handler, hello},
             {"/sample", api_handler, sample},
             {"/bet-list", api_handler, 'bet-list'},
             {"/enter-contract/:json", api_handler, 'enter-contract'},
             {"/clone-contract/:json", api_handler, 'clone-contract'},
             {"/submit-t2-signature/:json", api_handler, 'submit-t2-signature'},
             {"/get-t3-for-signing/", api_handler, 'get-t3-for-signing'},
             {"/submit-t3-signatures/:json", api_handler, 'submit-t3-signatures'}
            ]
           }
          ]),

    %% start cowboy json server
    {ok, _} = cowboy:start_http(json, 100, [{port, 8081}],
                                [
                                 {env, [{dispatch, JSONDispatch}]},
                                 {middlewares, [cowboy_router, cowboy_handler]}
                                ]),

    %% -------------------------------------------------------------------
    %% Web Site
    %% -------------------------------------------------------------------
    %% define http hosts, pathes, their patterns and handlers
    HTMLDispatch = cowboy_router:compile(
                     [
                      {'_', [
                             {"/index.html",    page_handler, {index}},
                             {"/about.html",    page_handler, {about}},
                             {"/intro.html",    page_handler, {intro}},
                             {"/bets.html",     page_handler, {bets}},
                             {"/details.html",  page_handler, {details}},
                             {"/flow.html",     page_handler, {flow}},
                             {"/prep/:id",      page_handler, {prep}},
                             {"/pend.html",     page_handler, {pend}},
                             {"/sign/:id",      page_handler, {sign}},
                             {"/followup.html", page_handler, {followup}},
                             {"/status/:id",    page_handler, {status}},
                             {"/events.html",   page_handler, {events}},
                             {"/cashout.html",  page_handler, {cashout}},
                             {"/cashout2.html", page_handler, {cashout2}},
                             {"/wrapup.html",   page_handler, {wrapup}},
                             {"/over.html",     page_handler, {over}},
                             {"/[...]", cowboy_static,
                              {priv_dir, middle_server, "",
                               [{mimetypes, cow_mimetypes, all}]}}
                            ]}
                     ]),

    %% start cowboy http server
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [
                                 {env, [{dispatch, HTMLDispatch}]},
                                 {middlewares, [cowboy_router, cowboy_handler]}
                                ]),

    %% start request-handling middleware
    middle_server_sup:start_link().

stop(_State) ->
    ok.
