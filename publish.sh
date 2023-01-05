set -e
git checkout ghpages
git merge main
rm -rf docs/
elm-app build
mv build/ docs/
git add docs/
git commit -m "Build"
git push
git checkout main
