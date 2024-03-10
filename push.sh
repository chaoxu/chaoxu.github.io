#!/bin/bash
# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop
git pull

# make sure node path contains global
export NODE_PATH=$(npm root --quiet -g)

# make sure katex_cli is here
cp ~/Documents/GitHub/katex_cli/target/release/katex_cli katex_cli
# Build new files
# stack build --ghc-options=-O2
# stack exec chaosite clean
stack exec chaosite build

# Build index
python3 pub.py > _site/index.html

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
rsync -a --checksum --filter='P _site/' --filter='P _cache/' --filter='P .git/' --filter='P .stack-work/' --filter='P .gitignore' --filter='P .gitattributes' --delete-excluded _site/ .
rm -r drafts

# Commit
touch .nojekyll
git add -A
git commit -m "Publish."

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
git stash pop
