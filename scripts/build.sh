#!/bin/bash
docker-compose run --rm app bash -c 'stack build && stack test --no-run-tests'
