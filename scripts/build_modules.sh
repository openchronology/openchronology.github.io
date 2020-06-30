#! /bin/bash

GRAPHMOD=./graphmod/.stack-work/dist/*/Cabal-2.2.0.1/build/graphmod/graphmod
MODULES=./graphs/modules.dot
MODULESOUT=./graphs/modules.png

TIMELINEDIR=./.spago/timeline/master/src/
TIMELINETIMEDIR=./.spago/timeline-time/master/src/

# build graph
if [ $# -eq 0 ]; then
    find src test -name '*.purs' | xargs $GRAPHMOD -q \
    -R 'MaterialUI.Icons' -R 'React' -R 'Scripts' -d 48,32 > $MODULES \
    || { exit 1; }
elif [ $1 == "verbose" ]; then
    find src test $TIMELINEDIR $TIMELINETIMEDIR -name '*.purs' | xargs $GRAPHMOD -q \
    -R 'MaterialUI.Icons' -R 'React' -R 'Scripts' -d 48,32 > $MODULES \
    || { exit 1; }
else
    echo "incorrect flag usage - either \"verbose\" or none"
    exit 1
fi
# rename graph
sed -i 's/digraph G/digraph Modules/' $MODULES
# redirect nodes - "is imported by"
sed -i 's/\(.*\) -> \(.*\);/\2 -> \1;/' $MODULES
# draw graph to svg
dot -Tpng $MODULES -o $MODULESOUT
