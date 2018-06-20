#!/bin/bash
docker-compose run --rm --no-deps app bash -c 'stack build --ghc-options "-Wall -fwarn-unused-imports" && stack test --no-run-tests'
