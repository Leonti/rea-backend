#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack --rm leonti/haskell-static-build:17.09.28.00.34

docker build -t leonti/rea-backend:$version .
docker push leonti/rea-backend:$version

echo $version" is built"
