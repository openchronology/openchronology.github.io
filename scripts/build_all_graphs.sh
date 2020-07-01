#! /bin/bash

SIGNALS=./graphs/signals-and-queues.dot
SIGNALSOUT=./graphs/signals-and-queues.svg

./scripts/build_modules.sh \
    || { exit 1; }
./scripts/build_modules.sh verbose \
    || { exit 1; }
dot -Tsvg $SIGNALS -o $SIGNALSOUT
