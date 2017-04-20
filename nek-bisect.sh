#!/usr/bin/env bash

git bisect start HEAD 4fe72d35e6ce225a6f0e194257fc88c93d69ec49 --
git bisect run python -m 'unittest' NekTests.$1
git bisect reset
