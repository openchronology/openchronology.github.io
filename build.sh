#!/bin/bash

JSTMP=./build/index.tmp.js
JS=./build/index.js
JSMIN=./build/index.min.js

TEMPLATE=./build/index.template.html
ROBOTO=./build/roboto.css
MATERIALICONS=./build/materialicons.css
FONTAWESOME=./build/fontawesome.css
FONTAWESOMEBRANDS=./build/brands.css

OUTPUT=./index.html

if [ $# -eq 0 ]; then
    JSOUT=$JS
elif [ $1 == "production" ]; then
    JSOUT=$JSMIN
else
    echo "incorrect flag usage - either \"production\" or none"
    exit
fi

pulp build || { exit 1; }
purs bundle output/**/*.js -m Main --main Main > $JSTMP || { echo "Bundle Failed"; exit 1; }
echo "Bundled"
./node_modules/.bin/browserify $JSTMP -o $JS -t [ babelify --presets [ @babel/preset-env ] --plugins [ @babel/plugin-proposal-class-properties ] ] || { exit 1; }
rm $JSTMP

# uglify only in production
if [ $# -eq 1 ] && [ $1 == "production" ]; then
    ./node_modules/.bin/browserify $JS -g [ envify --NODE_ENV production ] -g uglifyify | ./node_modules/.bin/uglifyjs --compress --mangle > $JSMIN || { exit 1; }
fi
echo "Browserified"

# final template
ltext "$TEMPLATE $JSOUT $ROBOTO $MATERIALICONS $FONTAWESOME $FONTAWESOMEBRANDS" \
      --raw $JSOUT --raw $ROBOTO --raw $MATERIALICONS \
      --raw $FONTAWESOME --raw $FONTAWESOMEBRANDS > $OUTPUT || { exit 1; }
echo "Finished"
