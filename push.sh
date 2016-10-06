#!/bin/bash
# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
mv ~/tmp/.stack-work .stack-work
stack exec chaosite clean
stack exec chaosite build
mv .stack-work/ ~/tmp/

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
