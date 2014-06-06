%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.3.x/web flow                                            %%%
%%% File        : page_handler.erl                                          %%%
%%% Description : web site page creation, as a handler for Cowboy           %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 24 May 2014                                               %%%
%%% Changed     : 06 June 2014                                              %%%
%%%-------------------------------------------------------------------------%%%
-module(page_handler).

%% REST Callbacks
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([page/2]).

%% ----------------------------------------------------------------------------
%% Cowboy Callbacks
%% ----------------------------------------------------------------------------
init(_Transport, _Req, _Paths) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Paths) ->
    {ok, Req, Paths}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"text">>, <<"html">>, []}, page}
     ], Req, State}.

%% ----------------------------------------------------------------------------
%% Page Creation
%% ----------------------------------------------------------------------------
%% assemble index.html
page(Req, State) ->
    Body = erlang:iolist_to_binary([
                                    block("head.html"),
                                    html(State),
                                    block("foot.html")]),
    HTML = [<<"<!DOCTYPE html><html><head><title>Index</title></head>",
              "<body>">>, Body, <<"</body></html>\n">>],
    {HTML, Req, somepath}. %% TODO somepath?

html({index}) ->
    bets_html(samples());

html({about}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({intro}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({bets}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({bet}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({flow}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({prep}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({pend}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({sign}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({followup}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({status}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({cashout}=State) ->
    placeholder(io_lib:format("~p", [State]));

html({wrapup}=State) ->
    placeholder(io_lib:format("~p", [State]));

html(State) ->
    placeholder(io_lib:format("~p ?", [State])).


%% ----------------------------------------------------------------------------
%% Bets Lists
%% ----------------------------------------------------------------------------

%% Create HTML that displays bet offerings.
bets_html(DataList) ->
    Template = block("bet.html"),
    [ bet_html(Template, Data) || Data <- DataList ].

bet_html(Template, Data) ->
    merge(Template, Data).


%% ----------------------------------------------------------------------------
%% Dynamic Pages
%% ----------------------------------------------------------------------------

%% load the HTML from a template block
block(Name) ->
    %% io:format("file path: ~p~n", [full_path(Name)]),
    {ok, Bin} = file:read_file(full_path(Name)),
    Bin.

%% create path to the HTML templates in the app's private folder
%% Note that this folder is under _rel and you CAN change HTML dynamically
%% to see changes right away, if you find the right folder.
full_path(Name) ->
    %% io:format("file dir: ~p~n", [code:priv_dir(middle_server)]),
    filename:join([code:priv_dir(middle_server), "blocks/" ++ Name]).

%% Sample data to be injected into the HTML
samples() ->
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
      {yes_amount, "10"},
      {no_amount, "3"},
      {yes_bidder, "Hans Langen"},
      {yes_pubkey, "#1dkuebmicbfviwkjnbepivavriongerjvdfkjn"},
      {no_bidder, "YOU?"},
      {no_pubkey, "--"},
      {smallprint, "small print"}]
    ].

%% Join a flat data structure and a HTML template.
%% E.g. merge(<<"<a href=hello.html>$HELLO</a>">>, [{hello, "Hej!"}])
%% results into <<"<a href=hello.html>Hej!</a>">>
merge(Template, []) ->
    Template;

merge(Template, [{Tag, String} | Data]) ->
    Search = "\\$" ++ string:to_upper(atom_to_list(Tag)),
    Replaced = re:replace(Template, Search, String, [global, {return, list}]),
    merge(Replaced, Data).

%% Placeholder for pages under construction
placeholder(S) ->
    "<h3>" ++ S ++ "</h3>".
