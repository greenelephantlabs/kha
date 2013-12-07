REBAR=$(shell which rebar || echo ./rebar)

.PHONY: default all deps normal fast clean test tests sh

default: normal

all: $(REBAR) deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

normal:
	$(REBAR) compile

fast:
	$(REBAR) compile skip_deps=true

clean:  $(REBAR)
	$(REBAR) clean
	make distclean

test: tests
tests:  $(REBAR)
	$(REBAR) eunit skip_deps=true

sh:
	erl -pa ebin/ deps/*/ebin/

ALL=    priv/coffee/app.js \
	priv/models/build.js \
	priv/controllers/builds_controller.js

%.js: %.coffee
	@coffee -c $<

priv/js/kha.js: $(ALL)
	cat $^ > $@

include dialyzer.mkf

# Detect or download rebar

REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
./rebar:
	erl -noshell -s inets -s ssl \
	-eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

distclean: dialyzer_distclean
	rm -f ./rebar

# For update code
up:
	git fetch
	git checkout origin/master
	make all
