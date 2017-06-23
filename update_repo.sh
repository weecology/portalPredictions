#!/bin/sh

git config --global user.email "weecologydeploy@weecology.org"
git config --global user.name "Weecology Deploy Bot"

git add predictions/* docs/*
git commit -m "Update forecasts: Travis Build $TRAVIS_BUILD_NUMBER"

git remote add deploy https://${GITHUB_TOKEN}@github.com/weecology/portalPredictions.git > /dev/null 2>&1
git push --quiet deploy master > /dev/null 2>&1
