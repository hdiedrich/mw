PROJECT = middle_server

DEPS = lager cowboy
dep_cowboy = pkg://cowboy master
dep_lager = https://github.com/basho/lager.git

include erlang.mk

run:
	./_rel/bin/middle_server-0.1 console
