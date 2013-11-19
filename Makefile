.PHONY : all clean eunit release

all:
	./rebar compile escriptize

clean:
	./rebar clean

eunit:
	./rebar eunit

release:
	./release.sh
