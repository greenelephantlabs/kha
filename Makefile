all:	rebar get-deps
	rebar compile

fast:	rebar compile

ALL=    priv/coffee/app.js \
	priv/models/build.js \
	priv/controllers/builds_controller.js

%.js: %.coffee
	@coffee -c $<

priv/js/kha.js: $(ALL)
	cat $^ > $@
