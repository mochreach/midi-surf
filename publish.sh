set -e
git checkout ghpages
git merge --no-edit main
# elm-app test
rm -rf docs/
NODE_OPTIONS="--openssl-legacy-provider" HTTPS="true" elm-app build
mv build/ docs/
git add docs/
git commit -m "Build" && git push
git checkout main
