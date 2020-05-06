#! /bin/bash

GRAPHMOD=./graphmod/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/graphmod/graphmod

# build graph
find src -name '*.purs' | xargs $GRAPHMOD -q \
  -R 'MaterialUI' -R 'React' -d 12,8 > modules.dot
# rename graph
sed -i 's/digraph G/digraph Modules/' modules.dot
# redirect nodes - "is imported by"
sed -i 's/\(.*\) -> \(.*\);/\2 -> \1;/' modules.dot
# draw graph to svg
dot -Tsvg modules.dot -o modules.svg
