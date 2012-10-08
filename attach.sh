#!/bin/sh
HOST=`hostname -f`
erl -name kha_shell -remsh kha@$HOST
