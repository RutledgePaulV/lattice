#!/usr/bin/env bash
set -e

clj -X:build clean
clj -X:build jar
version=$(clj -X:build get-version)

export CLOJARS_USERNAME="op://Personal/clojars.org/username"
export CLOJARS_PASSWORD="op://Personal/clojars.org/token"

op run -- mvn deploy:deploy-file \
  -DgroupId="io.github.rutledgepaulv" \
  -DartifactId="lattice" \
  -Dversion="$version" \
  -Dpackaging="jar" \
  -Dfile="target/lattice.jar" \
  -DrepositoryId="clojars" \
  -Durl="https://repo.clojars.org"

git tag "v$version"
git push origin "refs/tags/v$version"