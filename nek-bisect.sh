#!/usr/bin/env bash

git bisect start HEAD 92b9d5faed48798cbfb4c1b42edcbf546e7615cf --
git bisect run python -m 'unittest' NekTests.$1
git bisect reset
