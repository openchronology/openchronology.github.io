#! /bin/bash

./scripts/build_all_graphs.sh

# takes forever
node_modules/.bin/spago docs -f html \
  || { exit 1; }

# move all local docs
find src .spago/timeline/master/src .spago/timeline-time/master/src -name '*.purs' -printf '%P\n' \
  | sed 's/\.purs/.html/' \
  | sed 's/src\///' \
  | sed 's/\//./g' \
  | sed 's/\(.*\)/mv generated-docs\/html\/\1 generated-docs\/\1/' \
  | xargs -L 1 xargs -t

# remove other docs
rm -r generated-docs/html/

# process html, removing modules not in this package
node_modules/.bin/spago run -m Scripts.TrimDocs
