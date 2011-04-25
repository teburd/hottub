
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

build_plt:
	@./rebar skip_deps=true build-plt

dialyze:
	@./rebar skip_deps=true dialyze

eunit:
	rm -f .eunit/*.dat
	@./rebar skip_deps=true eunit

test: eunit

rel: rel/reltool.config rel_clean all
	@./rebar generate

rel_clean:
	rm -rf rel/hottub
