#!/bin/bash
# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop
# Abort loudly on a failed pull (e.g. divergent branches with pull.ff=only):
# continuing here would build from a stale tree and publish a regression.
git pull || { echo "ERROR: git pull failed. develop diverged from origin? Aborting before build."; git stash pop 2>/dev/null; exit 1; }

# make sure node path contains global
export NODE_PATH=$(npm root --quiet -g)

# make sure katex_cli is here
cp ~/Documents/GitHub/katex_cli/target/release/katex_cli katex_cli
# Build new files
# stack build --ghc-options=-O2
# stack exec chaosite clean
# Abort if the site build fails (e.g. chaosite not compiled for the current
# resolver): without this, _site is left stale and gets published silently.
stack exec chaosite build || { echo "ERROR: 'stack exec chaosite build' failed. Run 'stack build' under lts-20.26 first? Aborting."; git stash pop 2>/dev/null; exit 1; }

# make python environment
python3 -m venv venv
source venv/bin/activate
pip3 install -r requirements.txt
# Build index (write to a temp file first so a pub.py failure never leaves a
# truncated index.html behind).
python3 pub.py > _site/index.html.new || { echo "ERROR: pub.py failed. Aborting."; rm -f _site/index.html.new; git stash pop 2>/dev/null; exit 1; }
mv _site/index.html.new _site/index.html

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
