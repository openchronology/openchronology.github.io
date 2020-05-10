#! /bin/bash

SIGNALS=./graphs/signals-and-queues.dot
SIGNALSOUT=./graphs/signals-and-queues.svg

./scripts/build_modules.sh
dot -Tsvg $SIGNALS -o $SIGNALSOUT
