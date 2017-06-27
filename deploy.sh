eval "$(ssh-agent -s)" # Start the ssh agent
echo "$DEPLOY_KEY" > deploy_key.pem
chmod 600 deploy_key.pem # This key should have push access
ssh-add deploy_key.pem
git add docs/index.html
git commit -m "Automatic website update"
git push origin master
