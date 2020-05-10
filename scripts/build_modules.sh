#! /bin/bash

GRAPHMOD=./graphmod/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/graphmod/graphmod
MODULES=./graphs/modules.dot
MODULESOUT=./graphs/modules.svg

# build graph
find src -name '*.purs' | xargs $GRAPHMOD -q \
  -R 'MaterialUI' -R 'React' -d 12,8 > $MODULES
# rename graph
sed -i 's/digraph G/digraph Modules/' $MODULES
# redirect nodes - "is imported by"
sed -i 's/\(.*\) -> \(.*\);/\2 -> \1;/' $MODULES
# draw graph to svg
dot -Tsvg $MODULES -o $MODULESOUT
