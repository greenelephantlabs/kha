#!/bin/sh
cd `dirname $0`

NAME=kha

if [ "$1" = "" ]
then
    CONFIG="support/kha"
else
    CONFIG="$1"
fi

erl -name $NAME -pa ebin deps/*/ebin -boot start_sasl -s reloader -run kha_app -config $CONFIG
