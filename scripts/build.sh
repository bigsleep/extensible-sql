#!/bin/bash
docker-compose run --rm app bash -c 'stack build --ghc-options "-Wall -fno-warn-unused-imports" && stack test --no-run-tests'
