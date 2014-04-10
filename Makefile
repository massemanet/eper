REBAR = ./rebar

.PHONY : all doc clean eunit release xref

all:
	@$(REBAR) compile escriptize

clean:
	@$(REBAR) clean

eunit:
	@$(REBAR) eunit

xref: all
	@$(REBAR) xref

release: xref eunit
	./release.sh

docs:
	@$(REBAR) doc skip_deps=true
