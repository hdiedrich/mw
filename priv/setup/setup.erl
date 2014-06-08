-module(setup).

-export([run/0]).

%-define(WC, "football world cup 2014").
-define(WC, "WC '14").

%% server initialization: read file and test (it could have been changed).
run() ->
    io:format("start~n"),
    erlang:put(count, 1),
    write("index", <<"hello">>),
    results(teams("teams.csv"), matches("matches.csv")).

%% read and store the interval tuples, create tree.
matches(Filename) ->
    io:format("read: ", []),
    {ok, B} = file:read_file(Filename),
    io:format("~w bytes~n", [byte_size(B)]),

    io:format("grep: ", []),
    %% number, date, hour, home, guest, stage, headline, description
    {match, Strings} =
        re:run(B,"([^,]+), *([^,]+), *([^,]+), *([^,]+), *([^,]+), *([^,]+), *([^,]+), *(.+)\\n?",
            [global, {capture, all_but_first, list}]),
    io:format("~w matches~n", [length(Strings)]),

    io:format("list matches~n", []),
    [ io:format("#~s ~s ~s~n", [_Number, _Stage, _Headline])
    ||  [_Number, _Date, _Hour, _Home, _Guest, _Stage, _Headline, _Description]
        <- Strings ],
    Strings.

teams(Filename) ->
    io:format("read teams: ", []),
    {ok, B} = file:read_file(Filename),
    io:format("~w bytes~n", [byte_size(B)]),

    io:format("grep teams: ", []),
    %% country, points
    {match, Strings} =
        re:run(B,"([^,]+), *(.+)\\n?",
            [global, {capture, all_but_first, list}]),
    io:format("~w teams~n", [length(Strings)]),

    io:format("list teams~n", []),
    [ io:format("~s (group ~s)~n", Rec) ||  Rec <- Strings ],

    Strings.

results(Teams, Matches) ->

    io:format("create ...~n", []),

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

    io:format("~p offers~n", [length(Offers)]),
    io:format("sort ...~n", []),
    Sorted = lists:keysort(1, Offers),

    [ io:format("#~p ~s - ~s~n", [Number, Headline, Detail])
    || {Number, Prio, Headline, Detail} <- Sorted ],

    ok.

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
    ok = file:write_file(Filename, Bytes),
    io:format("Write ~w ~w bytes~n", [Filename, byte_size(Bytes)]).
