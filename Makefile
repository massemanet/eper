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

~/.dialyzer_plt:
	- dialyzer --output_plt ~/.dialyzer_plt --build_plt \
           --apps erts kernel stdlib crypto public_key inets eunit xmerl

dialyzer: ~/.dialyzer_plt
	dialyzer --plt ~/.dialyzer_plt -Wrace_conditions --src src

release_major: xref eunit
	./bin/release.sh major

release_minor: xref eunit
	./bin/release.sh minor

release: xref eunit
	./bin/release.sh patch
