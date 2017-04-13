REBAR = ./rebar3

.PHONY: all compile test clean
.PHONY: test eunit xref dialyze
.PHONY: release release_minor release_major release_patch

all: compile

compile:
	@$(REBAR) compile

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

test: compile eunit xref dialyze

eunit: all
	ERL_FLAGS="-sname eunit" $(REBAR) eunit

xref: all
	@$(REBAR) xref

dialyzer:
	@$(REBAR) dialyzer

release_major: test
	./bin/release.sh major

release_minor: test
	./bin/release.sh minor

release_patch: test
	./bin/release.sh patch

release: relase_patch
