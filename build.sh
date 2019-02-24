#!/bin/bash

JSTMP=./build/index.tmp.js
JS=./build/index.js
JSMIN=./build/index.min.js

TEMPLATE=./build/index.template.html

OUTPUT=./index.html

if [ $# -eq 0 ]; then
    JSOUT=$JS
elif [ $1 == "production" ]; then
    JSOUT=$JSMIN
else
    echo "incorrect flag usage - either \"production\" or none"
    exit
fi

pulp build && \
    purs bundle output/**/*.js -m Main --main Main > $JSTMP && \
    echo "Bundled" && \
    ./node_modules/.bin/browserify $JSTMP -o $JS -t [ babelify --presets [ @babel/preset-env ] --plugins [ @babel/plugin-proposal-class-properties ] ] && \
    rm $JSTMP && \
    if [ $# -eq 1 ] && [ $1 == "production" ]; then
        ./node_modules/.bin/browserify $JS -g [ envify --NODE_ENV production ] -g uglifyify | ./node_modules/.bin/uglifyjs --compress --mangle > $JSMIN || { exit 1; }
    fi
    echo "Browserified" && \
    ltext "$TEMPLATE $JSOUT" --raw $JSOUT > $OUTPUT && \
    echo "Finished"
