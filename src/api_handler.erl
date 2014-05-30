%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.1.x/initial spike                                       %%%
%%% File        : api_handler.erl                                           %%%
%%% Description : json response generation, as a handler for Cowboy         %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 29 May 2014                                               %%%
%%% Changed     : 30 May 2014                                               %%%
%%%-------------------------------------------------------------------------%%%

-module(api_handler).

%% REST Callbacks
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([response/2]).


%% ----------------------------------------------------------------------------
%% Cowboy Callbacks
%% ----------------------------------------------------------------------------

init(_Transport, _Req, _Paths) ->
        {upgrade, protocol, cowboy_rest}.

rest_init(Req, Paths) ->
        {ok, Req, Paths}.

allowed_methods(Req, State) ->
        {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, State) ->
        {true, Req, State}. 

content_types_provided(Req, State) ->
        {[
                {{<<"application">>, <<"json">>, []}, response},
                {{<<"text">>, <<"plain">>, []}, response}, 
                {{<<"text">>, <<"html">>, []}, response} 
        ], Req, State}.


%% ----------------------------------------------------------------------------
%% Responses
%% ----------------------------------------------------------------------------
%% The second parameter here is the third in the dispatch tuples of the hosts.

response(Req, hello=State) ->
        %- io:format("Req: ~p~n~n State:~p~n~n", [Req, State]),
        JSON = <<"{ \"hello\" : \"Hello, world!\" }">>,
        {JSON, Req, State};

response(Req, sample=State) ->
        io:format("Req: ~p~n~n State:~p~n~n", [Req, State]),
        JSON = json([{sample, sample1()}]),
        {JSON, Req, State};

response(Req, 'bet-list'=State) ->
        io:format("Req: ~p~n~n State:~p~n~n", [Req, State]),
        JSON = json([{'bet-list', sample2()}]),
        {JSON, Req, State}.


%% --------------------------------------------------------------------------- 
%% Sample data to be injected into the HTML
%% --------------------------------------------------------------------------- 

sample1() ->
        [[{ a, 1 }, { b, <<"b">> }, { c, c }]].

sample2() ->
        [
         [{bet, "Germany beat Brazil"},
          {yes_amount, "2"},
          {no_amount, "3"},
          {yes_bidder, "Hans Langen"},
          {yes_pubkey, "#1dkuebmicbfviwkjnbepivavriongerjvdfkjn"},
          {no_bidder, "YOU?"},
          {no_pubkey, "--"},
          {smallprint, "small print"}],
         [{bet, "Germany beat Brazil"},
          {yes_amount, "2"},
          {no_amount, "3"},
          {yes_bidder, "Hans Langen"},
          {yes_pubkey, "#1dkuebmicbfviwkjnbepivavriongerjvdfkjn"},
          {no_bidder, "YOU?"},
          {no_pubkey, "--"},
          {smallprint, "small print"}]
        ].


%% --------------------------------------------------------------------------- 
%% Mini JSON
%% --------------------------------------------------------------------------- 
%% Use http://jsonlint.com to verify output.

json([{_,_}|_]=L) ->
        "{" ++ jsonL(L) ++ "}";

json([H|_]=L) when not is_integer(H) ->
        "[" ++ jsonL(L) ++ "]";

json({Key, Value}) ->
        json(Key) ++ ":" ++ json(Value);

json(Term) when is_integer(Term) ->
        io_lib:format("~w", [Term]);

json(Term) ->
        io_lib:format("\"~s\"", [Term]).

jsonL(L) ->
        lists:droplast(lists:flatten([ json(E) ++ "," || E <- L ])).


