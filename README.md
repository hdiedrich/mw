AIX World Cup 2014 Middle Ware
==============================

The AIX WC14 concept enables football bets on the Bitcoin blockchain that are decentralized, oracle-driven contacts, requiring less trust.

     Description : Mw - AI Effect World Cup 2014 - Middle Server
     Version     : 0.3.x/JS stuff
     File        : README.md
     Copyright   : AI Effect Group, Berlin
     Author      : H. Diedrich <hd2010@eonblast.com>
     License     : MIT
     Created     : 24 May 2014
     Changed     : 06 June 2014

Status
------

This is the first complete 'Mw' stack of Cowboy, PostgreSQL and BitcoinJS. It has a minmal cowboy setup serving static pages in `priv/` and assembling dynamic pages from flat data structures and templates in `priv/blocks/`. PostgreSQL I don't know what it's doing at this point but it runs with the server down, which is good. And BitcoinJS is featured in the old de facto standard 0.1.3, with but a hello-js.html available as template at this point.

Requirements
------------

* Erlang ~ 16
* relx
* make
* git

PostgreSQL config
-----------------

This is currently in middle_server_app in lieu of being extracted to a config file:

``` erlang
 application:set_env(mw, pools,
                        [
                         {pgsql_pool, [{size, 1}, {max_overflow, 1}],
                          [
                           {host, "localhost"},
                           {dbname, "mw_alpha"},
                           {user, "mw"},
                           {pass, "mw"}
                          ]}
                        ]),
```

PostgreSQL bootstrap
--------------------
As a user with rights to modify the database (this could be postgres user):

``` bash
psql mw_alpha < priv/postgres/mw_db_drop_all
psql mw_alpha < priv/postgres/mw_db_init
```

Build & Run the Mw Stack
------------------------

To build this stack, run the following command:

``` bash
$ make
```

To start the server in the foreground:

``` bash
$ make run
```

If you don't have a PostgreSQL server running it will crash partially but still
serve pages.

Content
-------

Mw runs two http servers side by side. One for API2 serving JSON. And the web server for the main site, for information about the concept, and the functional prototype, including JS scripts.


### API2 JSON

There are currently

     http://localhost:8081/hello
     http://localhost:8081/sample
     http://localhost:8081/bet-list

The results are JSON objects. They are created in `api_handler.erl`. The matching of the URL is hard coded in the main dispatch rule, in `middle_server.erl` and matching atoms in `api_handler:response/2`.

enter-contract curl request:

``` bash
curl -v -H "Accept: application/json" -H "Content-type: application/json" -X GET -d '{"pubkey":"0457a6e187af6dcad28f678a92850610504aa64685b4d6f60cbc30c1a1407a0ce03df1d51102eb09aca7ca6df77c06fe3ef6054e2ee9dac7b5ac849f6e5c026b73"}'  http://localhost:8081/enter-contract/42
```


### Web Site

Try [http://localhost:8080/hello.html](http://localhost:8080/hello.html)

This page is served from priv/hello.html as is.


A sample of where we want to go is served as static page from
[http://localhost:8080/sample.html](http://localhost:8080/sample.html)


Check out [http://localhost:8080/index.html](http://localhost:8080/index.html)

This is the most meaningful example page served at this point. Is assembled from the template blocks in the `priv` folder: `head.html`, `foot.html`, `bet.html`. Note that the `priv` folder is copied into the release. You can change pages dynamically by changing these files but they are NOT the ones in the priv folder in this folder. They are somewhere under `_rel/`. When you change these, you can immediately reload in the browser.

`bet.html` is interesting as it contains uppercase, $-affixed placeholders for actual values. The actual creation of the html to be served is done in `src/page_handler.erl`.

The data injected into `bet.html` looks like this and is currently hardcoded. Note the atoms are being uppercased and $-affixed and matched against said placeholders in `bet.html`.

```
        [[{bet, "Germany beat Brazil"},
          {yes_amount, "2"},
          {no_amount, "3"},
          {yes_bidder, "Hans Langen"},
          {yes_pubkey, "#1dkuebmicbfviwkjnbepivavriongerjvdfkjn"},
          {no_bidder, "YOU?"},
          {no_pubkey, "--"},
          {smallprint, "small print"}]]
```

E.g. `<<"<a href=hello.html>$HELLO</a>">>, [{hello, "Hej!"}])` results into `<<"<a href=hello.html>Hej!</a>">>`.

The page `index.html` is kind of special cased with its own handler currently. The name is matched in full to chose the right handler. There is no inspection of parameters of a GET currently.

Both sample.html and index.html use the stylesheet in `priv/style.css`. It is served as static file.i


### BitcoinJS

Test BitcoinJS with [http://localhost:8080/hello-js.html](http://localhost:8080/hello-js.html)

This will give you a page with basic BitcoinJS operations like key creation,
hasing and signing.


