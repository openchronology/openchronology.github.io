#! /bin/bash

echo ""
echo ""
echo "Number of modules:"
find src/ .spago/timeline/master/src/ .spago/timeline-time/master/src/ -name "*.purs" | wc --lines

echo ""
echo ""
echo "Lines of Code per module:"
echo ""
echo ""
find src/ .spago/timeline/master/src/ .spago/timeline-time/master/src/ -name "*.purs" | xargs wc --lines | sort -nr

echo ""
spago run -m Scripts.ModuleImportsAndExports
