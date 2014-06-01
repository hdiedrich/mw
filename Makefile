PROJECT = middle_server

DEPS = lager cowboy
dep_cowboy = pkg://cowboy master
dep_lager  = https://github.com/basho/lager.git

ERLC_OPTS = +debug_info +export_all +'{parse_transform, lager_transform}' +'{lager_truncation_size, 16384}'

include erlang.mk

run:
	./_rel/bin/middle_server-0.1 console
