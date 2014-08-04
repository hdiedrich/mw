-module(mw_setup).

-compile(export_all).
-export([run/0]).

-include("log.hrl").

%-define(WC, "football world cup 2014").
-define(WC, "WC '14").

%% ensure unique temp files for concurrent threads
-define(SUFFIX, lists:flatten(string:tokens(pid_to_list(self()), "<>."))).

%% mw_setup:insert_world_cup_events().
insert_world_cup_events() ->
    erlang:put(count, 1),
    Results = results(teams("setup/teams.csv"),
                      matches("setup/matches.csv")),
    %% Quick hack to scale up concurrency a bit as CPU for key gen is bottleneck
    {FirstResults, _} = lists:split(length(Results) div 500, Results),
    Total = length(FirstResults),
    {Part1, Rest1} = lists:split(Total div 6, FirstResults),
    {Part2, Rest2} = lists:split(Total div 6, Rest1),
    {Part3, Rest3} = lists:split(Total div 6, Rest2),
    {Part4, Rest4} = lists:split(Total div 6, Rest3),
    {Part5, Part6} = lists:split(Total div 6, Rest4),
    Spawn =
        fun(Part) -> spawn(fun() ->
                                   erlang:put(mw_event_count, Total),
                                   lists:map(fun insert_wc_bet/1, Part)
                           end)
        end,
    lists:foreach(Spawn, [
                          Part1, Part2, Part3, Part4, Part5, Part6
                         ]),
    ok.

insert_wc_bet({_N, Headline, Detail}) ->
    {ok, EventPriv, EventPub,
     OracleYesPriv, OracleYesPub,
     OracleNoPriv, OracleNoPub} = gen_keys(),
    {ok, OracleKeysId} =
        mw_pg:insert_oracle_keys(OracleNoPub, OracleNoPriv,
                                 OracleYesPub, OracleYesPriv),
    {ok, EventId} = mw_contract:create_event(null, Headline, Detail,
                                             OracleKeysId,
                                             EventPriv, EventPub),
    {ok, ContractId} = mw_contract:create_contract(EventId),
    {ok, ECPubKey0} =
        file:read_file(filename:join(code:priv_dir(middle_server),
                                     "test_keys/giver_keys1/ec_pubkey")),
    ECPubKey = binary:replace(ECPubKey0, <<"\n">>, <<>>),
    {ok, RSAPubKey} =
        file:read_file(filename:join(code:priv_dir(middle_server),
                                     "test_keys/giver_keys1/rsa_pubkey.pem")),
    mw_contract:enter_contract(ContractId,
                               ECPubKey,
                               RSAPubKey,
                               %% As these are test givers for dev, we fake
                               %% the enc privkeys (they are never used)
                               <<"">>,
                               <<"">>
                              ),
    Total = erlang:get(mw_event_count),
    ?info("~p Inserted oracle_keys & event ~p (~s %)",
          [self(), OracleKeysId,
           io_lib:format("~.2f", [(OracleKeysId / Total) * 100])]),
    ok.

gen_keys() ->
    %% Experiment to block for quality entropy bits from /dev/random on Linux
    %% Turned out to take WAY too long to get enough bits to generate keys for
    %% all the bets.
    %% TODO: figure out if we can generate keys with this in a timely manner by
    %% adding enough entropy to Linux:
    %% TMPRandFile = "/tmp/mw_openssl_rand_file",
    %% SetOpenSSLRandFile =
    %%    "dd if=/dev/random bs=1 count=1024 of=" ++ TMPRandFile ++ "; "
    %%    "export RANDFILE=" ++ TMPRandFile,
    %% os:cmd(SetOpenSSLRandFile),

    {ok, OracleYesPriv, OracleYesPub} = gen_rsa_keypair(),
    {ok, OracleNoPriv, OracleNoPub}   = gen_rsa_keypair(),
    {ok, EventPriv, EventPub}         = gen_ec_keypair(),

    {ok, EventPriv, EventPub,
     OracleYesPriv, OracleYesPub,
     OracleNoPriv, OracleNoPub}.

gen_ec_keypair() ->
    BitcoinTool = filename:join(code:priv_dir(middle_server), "bitcoin-tool"),
    ECTempBytes = filename:join(code:priv_dir(middle_server),
                                "ec_temp_bytes_" ++ ?SUFFIX),
    ECPrivAbsPath = filename:join(code:priv_dir(middle_server),
                                  "temp_setup_ec_privkey" ++ ?SUFFIX),
    ECPubAbsPath = filename:join(code:priv_dir(middle_server),
                                 "temp_setup_ec_pubkey" ++ ?SUFFIX),

    GenBytes = "openssl rand 32 > " ++ ECTempBytes,

    %% https://en.bitcoin.it/wiki/Private_key#Base_58_Wallet_Import_format
    %% The privkey eventually gets parsed by client-side, so we want it in
    %% standard WIF so it works with e.g. bitcoinjs lib.
    %% TODO: change network back to bitcoin-testnet? needed mainent to work with
    %% bitcoinjs Bitcoin.ECKey.decodeString in the browser
    GenECPriv = BitcoinTool ++ " "
        "--network bitcoin "
        "--input-type private-key "
        "--input-format raw "
        "--input-file " ++ ECTempBytes ++ " "
        "--output-type private-key-wif "
        "--output-format base58check "
        "--public-key-compression uncompressed > " ++ ECPrivAbsPath,

    %% Pubkey is only used in T2 output script, so we use a format that can
    %% directly be used by Bj to put it into the script binary.
    %% TODO: verify! is the above true? should we change some parameter?
    GenECPub = BitcoinTool ++ " "
        "--network bitcoin "
        "--input-type private-key "
        "--input-format raw "
        "--input-file " ++ ECTempBytes ++ " "
        "--output-type public-key "
        "--output-format base58check "
        "--public-key-compression compressed > " ++ ECPubAbsPath,

    os:cmd(GenBytes),
    os:cmd(GenECPriv),
    os:cmd(GenECPub),

    {ok, ECPrivBin} = file:read_file(ECPrivAbsPath),
    {ok, ECPubBin} = file:read_file(ECPubAbsPath),

    ok = file:delete(ECTempBytes),
    ok = file:delete(ECPrivAbsPath),
    ok = file:delete(ECPubAbsPath),
    {ok, ECPrivBin, ECPubBin}.

gen_rsa_keypair() ->
    RSAPrivAbsPath = filename:join(code:priv_dir(middle_server),
                                   "temp_setup_rsa_privkey.pem_" ++ ?SUFFIX),
    RSAPubAbsPath = filename:join(code:priv_dir(middle_server),
                                  "temp_setup_rsa_pubkey.pem_" ++ ?SUFFIX),
    GenRSAPriv = "openssl genrsa -out " ++ RSAPrivAbsPath ++ " 2048",
    GenRSAPub  = "openssl rsa -in " ++ RSAPrivAbsPath ++
        " -pubout > " ++ RSAPubAbsPath,
    os:cmd(GenRSAPriv),
    os:cmd(GenRSAPub),
    {ok, RSAPrivBin} = file:read_file(RSAPrivAbsPath),
    {ok, RSAPubBin} = file:read_file(RSAPubAbsPath),
    ok = file:delete(RSAPrivAbsPath),
    ok = file:delete(RSAPubAbsPath),
    {ok, RSAPrivBin, RSAPubBin}.

%% server initialization: read file and test (it could have been changed).
run() ->
    %% io:format("start~n"),
    erlang:put(count, 1),
    write("index", <<"hello">>),
    results(teams("teams.csv"), matches("matches.csv")).

%% read and store the interval tuples, create tree.
matches(Filename) ->
    %% io:format("read: ", []),
    AbsPath = filename:join(code:priv_dir(middle_server), Filename),
    {ok, B} = file:read_file(AbsPath),
    %% io:format("~w bytes~n", [byte_size(B)]),

    %% io:format("grep: ", []),
    %% number, date, hour, home, guest, stage, headline, description
    {match, Strings} =
        re:run(B,"([^,]+), *([^,]+), *([^,]+), *([^,]+), *([^,]+), *([^,]+), *([^,]+), *(.+)\\n?",
            [global, {capture, all_but_first, list}]),
    %% io:format("~w matches~n", [length(Strings)]),

    %% io:format("list matches~n", []),
    [
     %% io:format("#~s ~s ~s~n", [_Number, _Stage, _Headline])
     ok
     ||  [_Number, _Date, _Hour, _Home, _Guest, _Stage, _Headline, _Description]
        <- Strings ],
    Strings.

teams(Filename) ->
    %% io:format("read teams: ", []),
    AbsPath = filename:join(code:priv_dir(middle_server), Filename),
    {ok, B} = file:read_file(AbsPath),
    %% io:format("~w bytes~n", [byte_size(B)]),

    %% io:format("grep teams: ", []),
    %% country, points
    {match, Strings} =
        re:run(B,"([^,]+), *(.+)\\n?",
            [global, {capture, all_but_first, list}]),
    %% io:format("~w teams~n", [length(Strings)]),

    %% io:format("list teams~n", []),
    %% [ io:format("~s (group ~s)~n", Rec) ||  Rec <- Strings ],

    Strings.

results(Teams, Matches) ->

    %% io:format("create ...~n", []),

    Offers = lists:flatten(
        [ [ results(Rec) ||  Rec <- Matches ],
          [ advance(Team) ||  Team <- Teams ],
          [ quarterfinals(Team) ||  Team <- Teams ],
          [ semifinals(Team) ||  Team <- Teams ],
          [ playfinale(Team) ||  Team <- Teams ],
          [ beat(Team1, Team2) ||  Team1 <- Teams, Team2 <- Teams, Team1 /= Team2 ],
          [ finale(Team1, Team2) ||  Team1 <- Teams, Team2 <- Teams, Team1 /= Team2 ],
          [ finale_pairing(Team1, Team2) ||  Team1 <- Teams, Team2 <- Teams, Team1 /= Team2 ],
          [ playoff(Team1, Team2) ||  Team1 <- Teams, Team2 <- Teams, Team1 /= Team2 ],
          [ winner(Team) ||  Team <- Teams ],
          [ unbeaten(Team) ||  Team <- Teams ],
          [ onewin(Team) ||  Team <- Teams ],
          [ onegoal(Team) ||  Team <- Teams ],
          [ noconcede(Team) ||  Team <- Teams ],
          [ groupwinner(Team) ||  Team <- Teams ],
          [ groupsecond(Team) ||  Team <- Teams ],
          [ grouplast(Team) ||  Team <- Teams ],
          [ runnerup(Team) ||  Team <- Teams ],
          [ third(Team) ||  Team <- Teams ]]),

    %% io:format("~p offers~n", [length(Offers)]),
    %% io:format("sort ...~n", []),
    Sorted = lists:keysort(1, Offers),

    % [
     % io:format("#~p ~s - ~s~n", [Number, Headline, Detail])
     % ok
    % || {Number, Prio, Headline, _Detail} <- Sorted ],

    [ {Number,
       lists:flatten(Headline),
       lists:flatten(Detail)}
      || {Number, _Prio, Headline, Detail} <- Sorted ].

results(Rec) ->
    [ result("beat", Rec, i),
      result("beat", Rec, i, invert),
      result("draw", Rec, i),
      [ result(io_lib:format("~p:~p", [X, Y]), Rec, iii) || X <- lists:seq(0,3), Y <- lists:seq(0,3) ]].


result(What, [_Number, _Date, _Hour, _Home, _Guest, _Stage, _Headline, _Description], Prio, invert) ->
        result(What, [_Number, _Date, _Hour, _Guest, _Home, _Stage, _Headline, _Description], Prio).

result(What, [_Number, _Date, _Hour, _Home, _Guest, _Stage, _Headline, _Description], Prio) ->
        { count(),
          Prio,
          io_lib:format("~s ~s ~s", [_Home, What, _Guest]),
          io_lib:format("~s game #~s ~s ~s/2014 ", [_Stage, _Number, _Headline, _Date]) }.

advance(Team) ->
    predict(ii, "advance", "advance from group stage to round of 16", Team).

quarterfinals(Team) ->
    predict(iii, "reach quarter-finals", "advance from round of 16 to quarter-finals", Team).

semifinals(Team) ->
    predict(iii, "reach semi-finals", "advance from quarter-finals to semi-finals", Team).

playfinale(Team) ->
    predict(ii, "play finale", "will play in finale", Team).

% refactor like advance
beat([Name1,_]=_Team1, [Name2,_]=_Team2) ->
    predict(ii,
            io_lib:format("~s knock out ~s", [Name1, Name2]),
            io_lib:format("~s will play and beat ~s in knock out stage, play-off or finale", [Name1, Name2])).

% refactor like advance
finale([Name1,_]=_Team1, [Name2,_]=_Team2) ->
    predict(iii,
            io_lib:format("~s win cup vs ~s", [Name1, Name2]),
            io_lib:format("~s will play and beat ~s in the finale", [Name1, Name2])).

% refactor like advance
winner([Name,_]=_Team) ->
    predict(i,
            io_lib:format("~s win cup", [Name]),
            io_lib:format("~s will win the world cup", [Name])).

% refactor like advance
groupwinner([Name,_]=_Team) ->
    predict(ii,
            io_lib:format("~s win group", [Name]),
            io_lib:format("~s will win their group", [Name])).

% refactor like advance
groupsecond([Name,_]=_Team) ->
    predict(ii,
            io_lib:format("~s 2nd in group", [Name]),
            io_lib:format("~s will finish second place in their group", [Name])).

% refactor like advance
grouplast([Name,_]=_Team) ->
    predict(iiii,
            io_lib:format("~s loose group", [Name]),
            io_lib:format("~s will finish as last in their group", [Name])).

% refactor like advance
unbeaten([Name,_]=_Team) ->
    predict(iiii,
            io_lib:format("~s will not loose", [Name]),
            io_lib:format("~s will win the world cup without loosing a game", [Name])).

% refactor like advance
onewin([Name,_]=_Team) ->
    predict(iiii,
            io_lib:format("~s win game", [Name]),
            io_lib:format("~s will win at least one of their games", [Name])).

% refactor like advance
onegoal([Name,_]=_Team) ->
    predict(iiii,
            io_lib:format("~s score", [Name]),
            io_lib:format("~s will score at least one goal in their games (own goals are scores of the other side as usual)", [Name])).

% refactor like advance
noconcede([Name,_]=_Team) ->
    predict(iiii,
            io_lib:format("~s won't concede in group", [Name]),
            io_lib:format("~s will not concede a goal in the group stages (own goals are scores of the other side as usual)", [Name])).

% refactor like advance
playoff([Name1,_]=_Team1, [Name2,_]=_Team2) ->
    predict(iii,
            io_lib:format("~s win 3rd vs ~s", [Name1, Name2]),
            io_lib:format("~s will play and beat ~s in the playoff game for third place", [Name1, Name2])).

% refactor like advance
finale_pairing([Name1,_]=_Team1, [Name2,_]=_Team2) ->
    predict(ii,
            io_lib:format("Finale ~s vs ~s", [Name1, Name2]),
            io_lib:format("The final game will be played out between ~s and ~s", [Name1, Name2])).

% refactor like advance
runnerup([Name,_]=_Team) ->
    predict(iiii,
            io_lib:format("~s 2nd", [Name]),
            io_lib:format("~s will loose the finale to win second place", [Name])).

% refactor like advance
third([Name,_]=_Team) ->
    predict(ii,
            io_lib:format("~s 3rd", [Name]),
            io_lib:format("~s will win the playoff game to win third place", [Name])).

%

predict(Prio, What, Detail) ->
        { count(), Prio, What, Detail }.

predict(Prio, What, Detail, [_Name, _Group]) ->
        { count(),
          Prio,
          io_lib:format("~s ~s", [_Name, What]),
          io_lib:format("~s ~s", [_Name, Detail]) }.

%% dirt

count() ->
    erlang:put(count, erlang:get(count) +1).

%% out

write(ID, Bytes) ->
    Filename = "site/" ++ ID ++ ".html",
    AbsPath = filename:join(code:priv_dir(middle_server), Filename),
    ok = file:write_file(AbsPath, Bytes),
    ok.
    %% io:format("Write ~w ~w bytes~n", [Filename, byte_size(Bytes)]).

intervals(Start, Stop, _) when Start > Stop ->
    [];
intervals(Start, Stop, N) when Start == Stop; (Start + N) > Stop ->
    [{Start, Stop}];
intervals(Start, Stop, N) ->
    [{Start, Start + N} | intervals(Start + N + 1, Stop, N)].
