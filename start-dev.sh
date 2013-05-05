#!/bin/sh
cd `dirname $0`

exec erl -pa `pwd`/ebin -pa `pwd`/deps/*/ebin -s term_cache
