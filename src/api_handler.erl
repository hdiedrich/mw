%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.6.x/json calls                                          %%%
%%% File        : api_handler.erl                                           %%%
%%% Description : json response generation, as a handler for Cowboy         %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 29 May 2014                                               %%%
%%% Changed     : 14 Jun 2014                                               %%%
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

-include("log.hrl").

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

content_types_accepted(Req, State) ->
        {[{{<<"application">>, <<"json">>, '*'}, handle_post}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, response},
      {{<<"text">>, <<"plain">>, '*'}, response},
      {{<<"text">>, <<"html">>, '*'}, response}
     ], Req, State}.

handle_post(Req, State) ->
    Body = <<"<h1>Football, hacking and beautiful women at room 77.</h1>">>,
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {true, Req2, State}.

%% ----------------------------------------------------------------------------
%% Responses
%% ----------------------------------------------------------------------------
%% The second parameter here is the third in the dispatch tuples of the hosts.
response(Req, hello=State) ->
    %% io:format("Req: ~p~n~n State:~p~n~n", [Req, State]),
    JSON = <<"{ \"hello\" : \"Hello, world!\" }">>,
    {JSON, Req, State};

response(Req, sample=State) ->
    io:format("Req: ~p~n~n State:~p~n~n", [Req, State]),
    JSON = json([{sample, sample1()}]),
    {JSON, Req, State};

response(Req, 'bet-list'=State) ->
    io:format("Req: ~p~n~n State:~p~n~n", [Req, State]),
    JSON = json([{'bet-list', sample2()}]),
    {JSON, Req, State};

response(Req, 'enter-contract'=State) ->
    io:format("Respone ...", []),
    HandleFun =
        fun() ->
                ?info("Req: ~p State:~p", [Req, State]),
                {JSON, _} = cowboy_req:binding('json', Req),
                ?info("JSON: ~p", [JSON]),
                {[{<<"contract_id">>, ContractId0},
                  {<<"ec_pubkey">>, ECPubKey},
                  {<<"rsa_pubkey">>, RSAPubKey}]} = jiffy:decode(JSON),
                ContractId = erlang:list_to_integer(
                               binary:bin_to_list(ContractId0)),
                Response = mw_contract:enter_contract(ContractId,
                                                      ECPubKey, RSAPubKey),
                ?info("Respone: ~p", [Response]),
                Response
        end,
    JSON = handle_response(HandleFun),
    ?info("Respone JSON: ~p", [JSON]),

    %% allow access from other port - actually from anywhere
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,
                                      <<"*">>, Req),
    {JSON, Req1, State};

response(Req, 'clone-contract'=State) ->
    HandleFun =
        fun() ->
                ?info("Req: ~p State:~p", [Req, State]),
                {JSON, _} = cowboy_req:binding('json', Req),
                {[{<<"contract_id">>, ContractId0}]} = jiffy:decode(JSON),
                ContractId = erlang:list_to_integer(
                               binary:bin_to_list(ContractId0)),
                Response = mw_contract:clone_contract(ContractId),
                ?info("Respone: ~p", [Response]),
                Response
        end,
    JSON = handle_response(HandleFun),
    ?info("Respone JSON: ~p", [JSON]),
    {JSON, Req, State};

response(Req, 'submit-t2-signature'=State) ->
    HandleFun =
        fun() ->
                ?info("Req: ~p State:~p", [Req, State]),
                {JSON, _} = cowboy_req:binding('json', Req),
                {[{<<"contract_id">>, ContractId0},
                  {<<"ec_pubkey">>, ECPubKey},
                  {<<"t2_signature">>, T2Signature}]} = jiffy:decode(JSON),
                ContractId = erlang:list_to_integer(
                               binary:bin_to_list(ContractId0)),
                Response = mw_contract:submit_t2_signature(ContractId, ECPubKey,
                                                           T2Signature),
                ?info("Respone: ~p", [Response]),
                Response
        end,
    JSON = handle_response(HandleFun),
    ?info("Respone JSON: ~p", [JSON]),
    {JSON, Req, State};

response(Req, 'get-t3-for-signing'=State) ->
    HandleFun =
        fun() ->
                ?info("Req: ~p State:~p", [Req, State]),
                {JSON, _} = cowboy_req:binding('json', Req),
                {[{<<"contract_id">>, ContractId0},
                  {<<"to_address">>, ToAddress}]} = jiffy:decode(JSON),
                ContractId = erlang:list_to_integer(
                               binary:bin_to_list(ContractId0)),
                Response =
                    mw_contract:get_t3_for_signing(ContractId, ToAddress),
                ?info("Respone: ~p", [Response]),
                Response
        end,
    JSON = handle_response(HandleFun),
    ?info("Respone JSON: ~p", [JSON]),
    {JSON, Req, State};

response(Req, 'submit-t3-signatures'=State) ->
    HandleFun =
        fun() ->
                ?info("Req: ~p State:~p", [Req, State]),
                {JSON, _} = cowboy_req:binding('json', Req),
                {[{<<"contract_id">>, ContractId0},
                  {<<"t3_raw">>, T3Raw},
                  {<<"t3_signature1">>, T3Signature1},
                  {<<"t3_signature2">>, T3Signature2}
                 ]} = jiffy:decode(JSON),
                ContractId = erlang:list_to_integer(
                               binary:bin_to_list(ContractId0)),
                Response = mw_contract:submit_t3_signatures(ContractId,
                                                            T3Raw,
                                                            T3Signature1,
                                                            T3Signature2),
                ?info("Respone: ~p", [Response]),
                Response
        end,
    JSON = handle_response(HandleFun),
    ?info("Respone JSON: ~p", [JSON]),
    {JSON, Req, State}.

%% Single, top-level try catch to ensure we return correct JSON error code / msg
%% for all handled errors, with a default for any unhandled error (crash).
%% This allows code deeper in the stack to be written in idiomatic Erlang style
%% for the correct case, without defensive coding.
handle_response(HandleFun) ->
    try
        json(HandleFun())
    catch throw:{api_error, {ErrorCode, ErrorMsg}} ->
            ?error("Handled API Error Code: ~p : ~p", [ErrorCode, ErrorMsg]),
            json([{"error-code", ErrorCode}, {"error-message", ErrorMsg}]);
          Error:Reason ->
            Stack = erlang:get_stacktrace(),
            ?error("Unhandled Error: ~p Reason: ~p Stack: ~p",
                   [Error, Reason, Stack]),
            json([{"error-code", 0},
                  {"error-message", "Unknown Error. "
                   "Something is on fire. Don't panic."}])
    end.

%% ---------------------------------------------------------------------------
%% Sample data to be injected into the HTML
%% -----------------------------------------------------------------------
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
