#!/usr/bin/env bash

git bisect start HEAD 7e564fcdbbfa91b4d2ff287cebc8d3498c8a689c --
git bisect run python -m 'unittest' NekTests.$1
git bisect reset
