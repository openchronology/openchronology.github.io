./build_all_graphs.sh

# takes forever
spago docs -f markdown

# move all local docs
find src -name '*.purs' \
  | sed 's/\.purs/.md/' \
  | sed 's/src\///' \
  | sed 's/\//./g' \
  | sed 's/\(.*\)/mv generated-docs\/md\/\1 generated-docs\/\1/' \
  | xargs -L 1 xargs -t

# remove other docs
rm -r generated-docs/md/
