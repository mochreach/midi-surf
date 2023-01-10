set -e
git checkout ghpages
git merge --no-edit main
elm-app test
rm -rf docs/
elm-app build
mv build/ docs/
git add docs/
git commit -m "Build" && git push
git checkout main
