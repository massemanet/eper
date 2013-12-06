.PHONY : all doc clean eunit release xref

all:
	./rebar compile escriptize

clean:
	./rebar clean

eunit:
	./rebar eunit

xref: all
	./rebar xref

release: xref eunit
	./release.sh

docs:
	./rebar doc skip_deps=true
