#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

sudo docker run -v "$(pwd):/src" --rm leonti/haskell-static-build

sudo docker build -t leonti/rea-backend:$version .
sudo docker push leonti/rea-backend:$version

echo $version" is built"
