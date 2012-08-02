REBAR=$(shell which rebar || echo ./rebar)

default: fast

all:    $(REBAR)
	$(REBAR) get-deps
	$(REBAR) compile

normal:
	$(REBAR) compile

fast:
	$(REBAR) compile skip_deps=true

clean:  $(REBAR)
	$(REBAR) clean
	make distclean

tests:  $(REBAR)
	$(REBAR) eunit

ALL=    priv/coffee/app.js \
	priv/models/build.js \
	priv/controllers/builds_controller.js

%.js: %.coffee
	@coffee -c $<

priv/js/kha.js: $(ALL)
	cat $^ > $@


# Detect or download rebar

REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
./rebar:
	erl -noshell -s inets -s ssl \
	-eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

distclean:
	rm -f ./rebar

# For update code
up:
	git fetch
	git checkout origin/master
	make all
