#!/usr/bin/env bash

git bisect start HEAD e4cd565b41aa27408c87cb44c8e6a7724a7662ab --
git bisect run python -m 'unittest' NekTests.$1
git bisect reset
