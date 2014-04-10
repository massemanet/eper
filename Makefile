REBAR = ./rebar

.PHONY: all clean eunit xref release release_minor release_major

all:
	@$(REBAR) compile escriptize

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

eunit:
	@$(REBAR) eunit

xref: all
	@$(REBAR) xref

release_major: xref eunit
	./bin/release.sh major

release_minor: xref eunit
	./bin/release.sh minor

release: xref eunit
	./bin/release.sh patch
