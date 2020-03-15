#!/bin/bash

JSTMP=./build/index.tmp.js
JS=./build/index.js
JSMIN=./build/index.min.js

TEMPLATE=./build/index.template.html
TEMPLATESTYLES=./build/staticstyles.template.html
TEMPLATESCRIPTS=./build/staticscripts.template.html
CSSROBOTO=./build/roboto.css
CSSMATERIALICONS=./build/materialicons.css
CSSFONTAWESOME=./build/fontawesome.css
CSSFONTAWESOMEBRANDS=./build/brands.css

OUTPUTDYNAMIC=./index.html
OUTPUTSTATIC=./static.html

STATICSTYLES=./build/staticstyles.html
STATICSCRIPTS=./build/staticscripts.html
DYNAMICSTYLES=./build/dynamicstyles.html
if [ $# -eq 0 ]; then
    DYNAMICSCRIPTS=./build/dynamicscripts.html
elif [ $1 == "production" ]; then
    DYNAMICSCRIPTS=./build/dynamicscripts.production.html
else
    echo "incorrect flag usage - either \"production\" or none"
    exit
fi

if [ $# -eq 0 ]; then
    JSOUT=$JS
elif [ $1 == "production" ]; then
    JSOUT=$JSMIN
fi

pulp build || { exit 1; }
purs bundle output/**/*.js -m Main --main Main \
     > $JSTMP || { echo "Bundle Failed"; exit 1; }
echo "Bundled"
./node_modules/.bin/browserify $JSTMP -o $JS \
                               -t [ babelify --presets [ @babel/preset-env ] \
                                             --plugins [ @babel/plugin-proposal-class-properties ] \
                                  ] || { exit 1; }
# clean up tmp
rm $JSTMP

# uglify only in production
if [ $# -eq 1 ] && [ $1 == "production" ]; then
    ./node_modules/.bin/browserify $JS -g [ envify --NODE_ENV production ] -g uglifyify | ./node_modules/.bin/uglifyjs --compress --mangle > $JSMIN || { exit 1; }
fi
echo "Browserified"

# templates
# static
ltext "$TEMPLATESTYLES $CSSROBOTO $CSSMATERIALICONS $CSSFONTAWESOME $CSSFONTAWESOMEBRANDS" \
      --raw $CSSROBOTO --raw $CSSMATERIALICONS --raw $CSSFONTAWESOME --raw $CSSFONTAWESOMEBRANDS \
      > $STATICSTYLES || { exit 1; }
ltext "$TEMPLATESCRIPTS $JSOUT" \
      --raw $JSOUT \
      > $STATICSCRIPTS || { exit 1; }

# final
ltext "$TEMPLATE $STATICSTYLES $STATICSCRIPTS" \
      --raw $STATICSTYLES --raw $STATICSCRIPTS \
      > $OUTPUTSTATIC || { exit 1; }
ltext "$TEMPLATE $DYNAMICSTYLES $DYNAMICSCRIPTS" \
      --raw $DYNAMICSTYLES --raw $DYNAMICSCRIPTS \
      > $OUTPUTDYNAMIC || { exit 1; }
echo "Templated"

# clean up static outputs
rm $STATICSTYLES
rm $STATICSCRIPTS

# static distribuition
mkdir static/
cp $OUTPUTSTATIC static/index.html
cp -r fonts/ static/fonts/

cd static/
zip -r ../openchronology-static.zip index.html fonts/ || { exit 1; }
cd ../

# clean up static dir
rm -r static/

echo "Finished"
