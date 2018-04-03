#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack --rm leonti/haskell-static-build:18.04.03.02.30

docker build -t leonti/rea-backend:$version .
docker push leonti/rea-backend:$version

echo $version" is built"
