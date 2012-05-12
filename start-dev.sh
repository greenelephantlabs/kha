#!/bin/sh
cd `dirname $0`

NAME=kha

erl -name $NAME -pa ebin deps/*/ebin -boot start_sasl -s reloader -eval "kha_app:start()."
