AIX World Cup 2014 Middle Ware
==============================

The AIX WC14 concept enables football bets on the Bitcoin blockchain that are decentralized, oracle-driven contacts, requiring less trust.  

     Description : Mw - AI Effect World Cup 2014 - Middle Server             
     Version     : 0.1.x/initial spike                                       
     File        : README.md
     Copyright   : AI Effect Group, Berlin                                   
     Author      : H. Diedrich <hd2010@eonblast.com>                         
     License     : MIT                                                       
     Created     : 24 May 2014                                               
     Changed     : 29 May 2014                                               

Status
------

This is a first spike relating the middle ware, a minmal cowboy setup serving static pages in `priv/` and assembling dynamic pages from flat data structures and templates in `priv/blocks/`.

Requirements
------------

* Erlang ~ 16
* relx
* make
* git

Build & Run
-----------

To build, run the following command:

``` bash
$ make
```

To start the server in the foreground:

``` bash
$ make run
```

Content
-------

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

Both sample.html and index.html use the stylesheet in `priv/style.css`. It is served as static file.
