#!/bin/bash

set -e

stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -O"
stack exec -- rea-backend +RTS -p -h
stack exec hp2ps rea-backend.hp
