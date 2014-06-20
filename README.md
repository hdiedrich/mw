AIX World Cup 2014 Middle Ware
==============================

The AIX WC14 concept enables football bets on the Bitcoin blockchain that are decentralized, oracle-driven contacts, requiring less trust.

     Description : Mw - AI Effect World Cup 2014 - Middle Server
     Version     : 0.6.x/JSON flow
     File        : README.md
     Copyright   : AI Effect Group, Berlin
     Author      : H. Diedrich <hd2010@eonblast.com>
     License     : MIT
     Created     : 24 May 2014
     Changed     : 21 June 2014

Status
------

This is the first complete 'Mw' stack of Cowboy, PostgreSQL and BitcoinJS. It has a minmal cowboy setup serving static pages in `priv/` and assembling dynamic pages from flat data structures and templates in `priv/blocks/`. PostgreSQL I don't know what it's doing at this point but it runs with the server down, which is good. And BitcoinJS is featured in the old de facto standard 0.1.3, with but a hello-js.html available as template at this point. There are 'blocks' for all
pages now that are part of the usage flow.

Requirements
------------

* git
* make
* Erlang R17*
* relx
* PostgreSQL

PostgreSQL config
-----------------

This is currently in middle_server_app in lieu of being extracted to a config file:

``` erlang
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
```

PostgreSQL bootstrap
--------------------
As a user with rights to modify the database (this could be postgres user):

``` bash
psql mw_alpha < priv/postgres/mw_db_drop_all
psql mw_alpha < priv/postgres/mw_db_init
```

Build & Run the Stack
---------------------

To build this stack, run the following command:

``` bash
$ make
```

Due to bleeding-edge hipster combination of GNU make, erlang.mk, relx, the erlang.mk package index, git dependencies resolved using rebar and some manual build steps, you have to run make a few more times to actually get everything built the very first time:

``` bash
$ make
$ make
$ make
```

To start the server in the foreground:

``` bash
$ make run
```

If you don't have a PostgreSQL server running it will crash partially but still serve pages.

Content
-------

Mw runs two http servers side by side. One for API2 serving JSON. And the web server for the main site, for information about the concept, and the functional prototype, including JS scripts.


### API2 JSON

The 'API2' is the interface between web site and Mw for communication between JS in the browser and the backend.

There are currently

     [http://localhost:8081/hello]
     [http://localhost:8081/sample]
     [http://localhost:8081/bet-list]
     [http://localhost:8081/enter-contract/<contract_id>]
     [http://localhost:8081/clone-contract/<contract_id>]
     [http://localhost:8081/submit-t2-signature/<contract_id>]
     [http://localhost:8081/get-t3-for-signing/<contract_id>]
     [http://localhost:8081/submit-t3-signatures/<contract_id>]

The results are JSON objects. They are created in `api_handler.erl`. The matching of the URL is hard coded in the main dispatch rule, in `middle_server.erl` and matching atoms in `api_handler:response/2`.

### Web Site

Try [http://localhost:8080/hello.html](http://localhost:8080/hello.html)

This page is served from priv/hello.html as is.


A sample of where we want to go is served as static page from
[http://localhost:8080/sample.html](http://localhost:8080/sample.html)


Check out [http://localhost:8080/index.html](http://localhost:8080/index.html)

This page is assembled from the template blocks in the `priv` folder: `head.html`, `foot.html`, `bet.html`. Note that the `priv` folder is copied into the release. You can change pages dynamically by changing these files but they are NOT the ones in the priv folder in this folder. They are somewhere under `_rel/`. When you change these, you can immediately reload in the browser.

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

Both sample.html and index.html use the stylesheet in `priv/style.css`. It is served as static file.

There is now a mechanism and stubs for all pages in the MVP #2 flow (see Pirate Pad). The flow is basically completely sequential, most of the time simply offering a 'continue' link, one page following the next.

When you have the server running on localhost, click through the pages to check them out.

 * [index.html](http://localhost:8080/index.html)       - landing page showing some bets
 * [about.html](http://localhost:8080/about.html)       - explanation of site and purpose
 * [intro.html](http://localhost:8080/intro.html)       - how to make a bet
 * [bets.html](http://localhost:8080/bets.html)         - list of available bets
 * [details.html](http://localhost:8080/details.html)   - detail information on one bet
 * [flow.html](http://localhost:8080/flow.html)         - instructions on how to close a bet
 * [prep.html](http://localhost:8080/prep.html)         - preparation of T1, with key creations
 * [pend.html](http://localhost:8080/pend.html)         - waiting page for T1, showing its status
 * [sign.html](http://localhost:8080/sign.html)         - signing of T2 with signing scripts
 * [followup.html](http://localhost:8080/followup.html) - explanation of what to do next
 * [status.html](http://localhost:8080/status.html)     - status page for T2
 * [events.html](http://localhost:8080/events.html)     - list of all events that have a decision
 * [cashout.html](http://localhost:8080/cashout.html)   - creation and signing of T3
 * [wrapup.html](http://localhost:8080/wrapup.html)     - status of T3 and congratulations
 * [over.html](http://localhost:8080/over.html)         - game over & thank you message for losers

We might want to cut one or two pages. But not now maybe.


### BitcoinJS

Test BitcoinJS with [http://localhost:8080/hello-js.html](http://localhost:8080/hello-js.html)

This will give you a page with basic BitcoinJS operations like key creation,
hasing and signing.

### Generate test keys

``` bash
openssl genrsa -out oracle_no_privkey.pem 2048
openssl rsa -in oracle_no_privkey.pem -pubout > oracle_no_pubkey.pem
```

``` bash
git clone https://github.com/matja/bitcoin-tool.git
make
sudo cp bitcoin-tool /usr/local/bin
openssl rand 32 > temp_bytes && bitcoin-tool --network bitcoin-testnet --input-type private-key --input-format raw --input-file temp_bytes --output-type private-key --output-format base58check --public-key-compression compressed > ec_privkey && bitcoin-tool --network bitcoin-testnet --input-type private-key --input-format raw --input-file temp_bytes --output-type public-key --output-format base58check --public-key-compression compressed > ec_pubkey && rm -f temp_bytes
```

### Example debug flow:

Reset database:

``` bash
psql mw < priv/postgres/mw_db_drop_all
psql mw < priv/postgres/mw_db_init
```

You will have to give all privileges again unless you don't.

Create some default events with dummy giver already entered (must do to call test pages prep/n etc):

``` erlang
mw_setup:insert_world_cup_events().
```

Enter taker:

``` bash
curl -v -H "Accept: application/json" -H "Content-type: applicatjson" "http://127.0.0.1:8081/enter-contract/\{\"contract_id\":\"5\",\"ec_pubkey\":\"6AymbcmHNSVXXcYh2HQDqNZc4HGHL2GwaTTYKDNgZtiDJvZQXN\",\"rsa_pubkey\":\"-----BEGIN%20PUBLIC%20KEY-----%5Cr%5CnMIGeMA0GCSqGSIb3DQEBAQUAA4GMADCBiAKBgHuMkwMNQwToUtGApry8GyuBlOpv%5Cr%5CnJBKtKnKh46VWeLWziIV%2BPve2PdoOBB9%2BHCHYZwAcHDJe5%2FV0bN1RsCZk6WryIFx5%5Cr%5Cn3LfrTR7vp48QZARiPwVAQzkfRwBxw9TqddubngASXMdvVkvZJNNKGKqCipIiETuR%5Cr%5Cn%2BvbexI5HKBLt86fdAgMBAAE%3D%5Cr%5Cn-----END%20PUBLIC%20KEY-----%5Cr%5Cn\"\}" 
```

This function should be called by the website when updating the contract page. This calls Bj to build T2 and then adds it to SQL:
``` erlang
mw_contract:get_contract_t2_state(1)
```

Submit t2 signature. Call this twice so there are two signatures added.

``` bash
curl -v -H "Accept: application/json" -H "Content -X GET -d '{"ec_pubkey":"6Vt5STpVk1MNmgNLUQjTsNrQYfF5viLUJHLGG3NqXvTjLwa3KT", "t2_signature":"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"}'  http://localhost:8081/submit-t2-signature/1
```

Manually add the event outcome. This function will later be called through e.g. some authenticated (perhaps EC signed?) API by the oracle:

``` erlang
mw_contract:add_event_outcome(1, true).
```

Get t3 for signing:
``` bash
curl -v -H "Accept: application/json" -H "Content-type: application/json" -X GET -d '{"to_address":"mrQ8iqcBfkTz1YVR3b4nhcGgEM6Yt7DwkD"}'  http://localhost:8081/get-t3-for-signing/1

```

Submit t3 signatures:

``` bash
curl -v -H "Accept: application/json" -H "Content-type: application/json" -X GET -d '{"t3_raw":"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF","t3_signature1":"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", "t3_signature2":"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"}'  http://localhost:8081/submit-t3-signatures/1
```
