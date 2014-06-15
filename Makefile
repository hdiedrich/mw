PROJECT = middle_server

DEPS = lager cowboy epgsql poolboy jiffy lhttpc proper bitcoin-tool

dep_lager  = https://github.com/basho/lager.git
dep_cowboy = pkg://cowboy master
dep_epgsql  = https://github.com/Gustav-Simonsson/epgsql.git
dep_poolboy = https://github.com/devinus/poolboy.git
dep_jiffy = https://github.com/davisp/jiffy.git
dep_lhttpc = https://github.com/Gustav-Simonsson/lhttpc.git
dep_proper = https://github.com/manopapad/proper.git

dep_bitcoin-tool = https://github.com/matja/bitcoin-tool.git

ERLC_OPTS = +debug_info +export_all +'{parse_transform, lager_transform}' +'{lager_truncation_size, 16384}'

include erlang.mk

all:
	cp deps/bitcoin-tool/bitcoin-tool priv/

run:
	./_rel/bin/middle_server-0.1 console
