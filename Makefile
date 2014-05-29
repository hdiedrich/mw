PROJECT = middle_server

DEPS = cowboy
dep_cowboy = pkg://cowboy master

include erlang.mk

run:
	./_rel/bin/middle_server-0.1 console
