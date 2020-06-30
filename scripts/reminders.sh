#! /bin/bash

echo ""
echo ""
echo "TODOs ---------------------------- "
echo ""
echo ""

ag "TODO" src/ .spago/timeline/master/src/ .spago/timeline-time/master/src/
echo "total matches: "
echo `ag "TODO" src/ ./.spago/timeline/master/src/ .spago/timeline-time/master/src/ --count --nofilename | sed "s/$/+/"`0 | bc

echo ""
echo ""
echo "FIXMEs --------------------------- "
echo ""
echo ""

ag "FIXME" src/ .spago/timeline/master/src/ .spago/timeline-time/master/src/
echo "total matches: "
echo `ag "FIXME" src/ ./.spago/timeline/master/src/ .spago/timeline-time/master/src/ --count --nofilename | sed "s/$/+/"`0 | bc

echo ""
echo ""
echo "throws --------------------------- "
echo ""
echo ""

ag "throw" src/ .spago/timeline/master/src/ .spago/timeline-time/master/src/
echo "total matches: "
echo `ag "throw" src/ ./.spago/timeline/master/src/ .spago/timeline-time/master/src/ --count --nofilename | sed "s/$/+/"`0 | bc
