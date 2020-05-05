#! /bin/bash

./build_modules.sh
dot -Tsvg signals-and-queues.dot -o signals-and-queues.svg
