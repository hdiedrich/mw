%%%-------------------------------------------------------------------------%%%
%%% Description : Mw - AI Effect World Cup 2014 - Middle Server             %%%
%%% Version     : 0.4.x/dynamic pages                                       %%%
%%% File        : page_handler.erl                                          %%%
%%% Description : web site page creation, as a handler for Cowboy           %%%
%%% Copyright   : AI Effect Group, Berlin                                   %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 24 May 2014                                               %%%
%%% Changed     : 12 June 2014                                              %%%
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
    {Block} = State,
    Title = erlang:iolist_to_binary("AIX WC 14 - " ++ atom_to_list(Block)),
    Body  = erlang:iolist_to_binary([
                                     block("head.html"),
                                     html(Req, State),
                                     block("foot.html")]),
    HTML = [<<"<!DOCTYPE html><html><head><title>">>,
            Title,
            <<"</title></head><body>">>,
            Body,
            <<"</body></html>\n">>],
    {HTML, Req, somepath}. %% TODO somepath?

html(_Req, {bets}=_State) ->
    Bin = block(bets),
    {ok, Data} = mw_pg:select_contract_infos(),
    merge(Bin, [{betlist, bets_html(Data)}]);

html(_Req, {index}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {about}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {intro}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {bets}=State) ->
    {Block} = State,
    Bin = block(Block),
    Bin2 = merge(Bin, [{betlist, bets_html(samples())}]),
    Bin2;

html(_Req, {details}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {flow}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {prep}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {pend}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {sign}=State) ->
    {Block} = State,
    block(Block);

html(_Req, {followup}=State) ->
    {Block} = State,
    block(Block);

html(Req, {status}=_State) ->
    {Id, _} = cowboy_req:binding(id, Req, none),
    case Id of
      none ->
        "Enter ID ... ";
      _ ->
        case mw_contract:get_contract_info(binary_to_integer(Id)) of
          {ok, Props} ->
            Headline = proplists:get_value("headline", Props, <<"?">>),
            History = events_to_html(proplists:get_value("history", Props)),
            merge(block(status),
              [{headline, Headline},
               {status, History},
               {dump, prop_to_html(Props)}
             ])
        end
    end;

html(_Req, {cashout}=State) ->
    {Block} = State, block(Block);

html(_Req, {wrapup}=State) ->
    {Block} = State, block(Block);

html(_Req, {over}=State) ->
    {Block} = State, block(Block);

html(_Req, State) ->
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
block(Name) when is_atom(Name)->
    File = atom_to_list(Name) ++ ".html",
    block(File);

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
     [{bet, "Honduras beat England"},
      {yes_amount, "1"},
      {no_amount, ".1"},
      {yes_bidder, "Bertl Gust"},
      {yes_pubkey, "#yTgtYhj64dggryew2bd32131141ngerjvdf342"},
      {no_bidder, "YOU?"},
      {no_pubkey, "--"},
      {smallprint, "small print"}]
    ].

%% Join a flat data structure and a HTML template.
%% E.g. merge(<<"<a href=hello.html>$HELLO</a>">>, [{hello, "Hej!"}])
%% results into <<"<a href=hello.html>Hej!</a>">>
merge(Template, []) ->
    Template;

merge(Template, [{Tag, Value} | Data]) ->
    Search = "\\$" ++ string:to_upper(to_list(Tag)),
    String = to_list(Value),
    io:format("search ~p~n", [Search]),
    Replaced = re:replace(Template, Search, String, [global, {return, list}]),
    merge(Replaced, Data).

to_list(A) when is_list(A) ->
    A;

to_list(A) when is_atom(A) ->
    atom_to_list(A);


to_list(A) when is_atom(A) ->
    atom_to_list(A);

to_list(A) when is_binary(A) ->
    binary_to_list(A);

to_list(A) when is_integer(A) ->
    integer_to_list(A).

%% Placeholder for pages under construction
placeholder(S) ->
    "<h3>" ++ S ++ "</h3>".

%% dump a prop list
prop_to_html(Prop) ->
    io_lib:format("<pre>~p</pre>", [Prop]).

%% make a html list from the contract events as they come from the DB 
events_to_html([]) -> [];

events_to_html([P | L]) ->
    [<<"<p> ">>,
     proplists:get_value("timestamp", P, <<"">>),
     <<": ">>,
     proplists:get_value("event", P, <<"">>),
     <<" <\p>">>] ++
     events_to_html(L).

