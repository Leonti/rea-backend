#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
sudo docker run -v "$(pwd):/src" --rm leonti/haskell-static-build:17.09.27.22.54

sudo docker build -t leonti/rea-backend:$version .
sudo docker push leonti/rea-backend:$version

echo $version" is built"
