
.PHONY: all compile deps eunit rel clean

all: compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@./rebar skip_deps=true docs

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.corbel_dialyzer_plt

build_plt:
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) deps/*/ebin apps/*/ebin ebin

dialyzer:
	@echo
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) apps/*/ebin deps/*/ebin ebin

typer:
	typer --plt $(COMBO_PLT) -r apps -I deps -I apps

eunit:
	rm -f .eunit/*.dat
	@./rebar skip_deps=true eunit

test: eunit

rel: rel/reltool.config rel_clean all
	@./rebar generate

rel_clean:
	rm -rf rel/hottub
