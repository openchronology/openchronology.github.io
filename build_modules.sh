#! /bin/bash

find src -name '*.purs' | xargs graphmod -q > modules.dot
sed -i 's/size="6,4"/size="12,8"/' modules.dot
sed -i 's/\(.*\) -> \(.*\);/\2 -> \1;/' modules.dot
dot -Tsvg modules.dot -o modules.svg
