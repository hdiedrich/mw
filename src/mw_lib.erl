%%%-------------------------------------------------------------------
%%% @author Gustav Simonsom <gustav.simonson@gmail.com>
%%% @copyright (C) 2014, AI Effect Group, Berlin
%%% @doc
%%% PostgreSQL / epgsql query utility functions
%%% @end
%%% Created : 06 Jun 2014 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(mw_lib).

-compile(export_all).
%% API
-export([]). %% TODO: remove export_all and add API exports

-include("log.hrl").

%%%===========================================================================
%%% API
%%%===========================================================================
hex_to_bin(L) when is_list(L) -> hex_to_bin(binary:list_to_bin(L));
hex_to_bin(B) when is_binary(B) ->
    <<<<(list_to_integer([D,D2], 16))>> || <<D,D2>> <= B>>.

bin_to_hex(B) when is_binary(B) ->
    <<<<(binary:list_to_bin(case length(S = integer_to_list(I, 16)) of
                                1 -> [$0|S];
                                2 -> S
                            end))/bytes>> || <<I>> <= B>>.

%%%===========================================================================
%%% Internal functions
%%%===========================================================================
