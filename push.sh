#!/bin/bash
# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# useful for no reason on MAC
# related to regex I used
export LANG=C
export LC_CTYPE=C

# Build new files
stack build
stack exec chaosite clean
stack exec chaosite build

# Build index
python pub.py > _site/index.html
# KaTeX local html filter
FILES=_site/**/*.html
for f in $FILES
do
  node math_katex_offline.js $f
done

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
rsync -a --filter='P _site/' --filter='P _cache/' --filter='P .git/' --filter='P .gitignore' --filter='P .gitattributes' --delete-excluded _site/ .
rm -r drafts

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
git stash pop
