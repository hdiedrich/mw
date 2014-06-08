.beam: *.erl
	erlc +debug_info $<

all: 
	erlc setup.erl
	@(erl -s setup run -s init stop -noshell)
