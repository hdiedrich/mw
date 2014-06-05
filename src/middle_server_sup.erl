%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.1.x/initial spike                                       %%%
%%% File        : middle_server_sup.erl                                     %%%
%%% Description : middle server supervision                                 %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 24 May 2014                                               %%%
%%% Changed     : 27 May 2014                                               %%%
%%%-------------------------------------------------------------------------%%%
-module(middle_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor start
init([]) ->
    Procs = [
             ?CHILD(mw_pg_sup, supervisor)
            ],
    {ok, {{one_for_one, 10, 10}, Procs}}.
