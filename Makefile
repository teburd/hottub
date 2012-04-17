.PHONY: all compile deps eunit rel clean

PROJECT = hottub
DIALYZER = dialyzer
REBAR = rebar

all: compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@$(REBAR) skip_deps=true docs

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler

build_plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl erts

dialyzer:
	@$(DIALYZER) --plt .$(PROJECT).plt ebin --no_native \
		-Wno_return -Werror_handling -Wrace_conditions -Wunmatched_returns

typer:
	typer --plt .$(PROJECT).plt src

eunit:
	rm -f .eunit/*.dat
	@$(REBAR) skip_deps=true eunit

test: eunit

rel: rel/reltool.config rel_clean all
	@$(REBAR) generate

rel_clean:
	rm -rf rel/hottub
