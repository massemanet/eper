.PHONY : all doc clean eunit release

all:
	./rebar compile escriptize

clean:
	./rebar clean

eunit:
	./rebar eunit

release:
	./release.sh

docs:
	./rebar doc skip_deps=true
