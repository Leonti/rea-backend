#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack -w /src --rm haskell:8.2.2 stack build --allow-different-user -- .

docker build -t leonti/rea-backend:$version .
docker push leonti/rea-backend:$version

echo $version" is built"
