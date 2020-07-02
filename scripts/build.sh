#!/bin/bash


# These environment variables just define the commonly used file paths
# when building and stitching everything together.


JSTMP=./build/index.tmp.js
JSTMPUGLY=./build/index.ugly.tmp.js
JS=./build/index.js
JSBABEL=./build/index.babel.js
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

LTEXT=./ltext/.stack-work/dist/*/Cabal-2.2.0.1/build/ltext/ltext

# Dynamic scripts will use external CDN sources when in production,
# but local ones when built statically

if [ $# -eq 0 ]; then
    DYNAMICSCRIPTS=./build/dynamicscripts.html
elif [ $1 == "production" ]; then
    DYNAMICSCRIPTS=./build/dynamicscripts.production.html
else
    echo "incorrect flag usage - either \"production\" or none"
    exit 1
fi

# $JSOUT is the target used when stitching into the template

if [ $# -eq 0 ]; then
    JSOUT=$JS
elif [ $1 == "production" ]; then
    JSOUT=$JSMIN
fi



# ----------- compiling

node_modules/.bin/spago bundle-app -m Main --to $JSTMP \
     || { echo "Bundle Failed"; exit 1; }

echo "Bundled"

# ----------- update module graph

./scripts/build_modules.sh \
     || { echo "Module Graph Failed"; exit 1; }
./scripts/build_modules.sh verbose \
     || { echo "Module Graph Failed"; exit 1; }
echo "Module Graph Built"



# Manually browserify the bundled output, because there's some
# ECMAScript here that needs to be processed, because of Material-UI

./node_modules/.bin/browserify $JSTMP -o $JSBABEL \
   -t [ babelify --presets [ @babel/preset-env @babel/preset-react ] \
                 --plugins [ @babel/plugin-proposal-class-properties @babel/plugin-transform-block-scoping ] \
      ] || { exit 1; }
echo "Browserified"

./node_modules/.bin/babel $JSBABEL > $JS \
    || { exit 1; }
rm $JSBABEL
echo "Re-ran Babel to get rid of unreasonable artifacts"

# uglify only in production, first through browserify
if [ $# -eq 1 ] && [ $1 == "production" ]; then
    ./node_modules/.bin/browserify $JS -g \
      [ envify --NODE_ENV production ] -g uglifyify > $JSTMPUGLY || \
      { exit 1; }
    echo "Uglified through Browserify"
    ./node_modules/.bin/uglifyjs $JSTMPUGLY --compress --mangle > $JSMIN || \
      { exit 1; }
    echo "Uglified total output"
    rm $JSTMPUGLY
fi

# clean up tmp
rm $JSTMP

# ---------- templates

# static
$LTEXT "$TEMPLATESTYLES $CSSROBOTO $CSSMATERIALICONS $CSSFONTAWESOME $CSSFONTAWESOMEBRANDS" \
      --raw $CSSROBOTO --raw $CSSMATERIALICONS --raw $CSSFONTAWESOME --raw $CSSFONTAWESOMEBRANDS \
      > $STATICSTYLES || { exit 1; }
$LTEXT "$TEMPLATESCRIPTS $JSOUT" \
      --raw $JSOUT \
      > $STATICSCRIPTS || { exit 1; }

# final
$LTEXT "$TEMPLATE $STATICSTYLES $STATICSCRIPTS" \
      --raw $STATICSTYLES --raw $STATICSCRIPTS \
      > $OUTPUTSTATIC || { exit 1; }
$LTEXT "$TEMPLATE $DYNAMICSTYLES $DYNAMICSCRIPTS" \
      --raw $DYNAMICSTYLES --raw $DYNAMICSCRIPTS \
      > $OUTPUTDYNAMIC || { exit 1; }
echo "Templated"

# clean up static outputs
rm $STATICSTYLES
rm $STATICSCRIPTS



# --------- finalization

# static distribuition
mkdir ./static/
cp $OUTPUTSTATIC ./static/index.html
cp -r ./fonts/ ./static/fonts/
cp -r ./images/ ./static/images/

cd ./static/
zip -q -r ../openchronology-static.zip index.html fonts/ || { exit 1; }
cd ../

# clean up static dir
rm -r ./static/

echo "Finished"
