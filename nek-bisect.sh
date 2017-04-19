#!/usr/bin/env bash

git bisect start 9ccfe16645f4f24a9c0a36db4ffd22725636e53d 978661850559886fa2e60da8da04296183a0ea21 --
git bisect run python -m 'unittest' NekTests.$1
git bisect reset
