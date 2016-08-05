#!/usr/bin/env sh

mkdir -p ./tmp
aperf --build ./build.sh --eval ./eval.py --results-file training.exhaustive.results sum.ml
aperf --build ./build.sh --eval ./eval.py --explore --results-file training.explore.results sum.ml
