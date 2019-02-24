#!/bin/bash

JSTMP=./build/index.tmp.js
JS=./build/index.js
JSMIN=./build/index.min.js

TEMPLATE=./build/index.template.html

OUTPUT=./index.html

pulp build && \
    purs bundle output/**/*.js -m Main --main Main > $JSTMP && \
    echo "Bundled" && \
    ./node_modules/.bin/browserify $JSTMP > $JS && \
    rm $JSTMP && \
    ./node_modules/.bin/browserify $JS -g [ envify --NODE_ENV production ] -g uglifyify | ./node_modules/.bin/uglifyjs --compress --mangle > $JSMIN && \
    echo "Browserified" && \
    ltext "$TEMPLATE $JSMIN" --raw $JSMIN && \
    echo "Finished"
