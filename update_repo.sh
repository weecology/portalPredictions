last_commit_date=`git log --date=iso --author='Weecology Deploy Bot' -1 --format=%cd | head -c 10`
current_date=`date -I | head -c 10`
if [ "$last_commit_date" !=  "$current_date" ]; then
    git config --global user.email "weecologydeploy@weecology.org"
    git config --global user.name "Weecology Deploy Bot"

    git checkout master
    git add predictions/* docs/*
    git commit -m "Update forecasts: Travis Build $TRAVIS_BUILD_NUMBER"

    git remote add deploy https://${GITHUB_TOKEN}@github.com/weecology/portalPredictions.git > /dev/null 2>&1
    git push --quiet deploy master > /dev/null 2>&1

    # Create a new release to trigger automated Zenodo archiving
    git tag $current_date
    git push --quiet deploy --tags > /dev/null 2>&1
    curl -v -i -X POST -H "Content-Type:application/json" -H "Authorization: token $GITHUB_RELEASE_TOKEN" https://api.github.com/repos/weecology/portalPredictions/releases -d "{'tag_name':$current_date}"
fi
