# Archive forecasts by pushing weekly forecasts to GitHub and tagging a
# release so that the GitHub-Zenodo integration archives the forecasts to
# Zenodo

# Only releases on cron driven events so that only weekly forecasts and not
# simple changes to the codebase triggers archiving.

if [ "$TRAVIS_EVENT_TYPE" == "cron" ]; then
    git config --global user.email "weecologydeploy@weecology.org"
    git config --global user.name "Weecology Deploy Bot"

    git checkout master
    git add predictions/* docs/* data/*
    git commit -m 'Update forecasts: Travis Build $TRAVIS_BUILD_NUMBER [ci skip]'

    git remote add deploy https://${GITHUB_TOKEN}@github.com/weecology/portalPredictions.git > /dev/null 2>&1
    git push --quiet deploy master > /dev/null 2>&1

    # Create a new release to trigger automated Zenodo archiving
    current_date=`date -I | head -c 10`
    git tag $current_date
    git push --quiet deploy --tags > /dev/null 2>&1
    curl -v -i -X POST -H "Content-Type:application/json" -H "Authorization: token $GITHUB_RELEASE_TOKEN" https://api.github.com/repos/weecology/portalPredictions/releases -d "{\"tag_name\":\"$current_date\"}"
fi
