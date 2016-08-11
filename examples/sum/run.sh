#!/usr/bin/env sh

mkdir -p ./tmp
echo exhaustive search
aperf --build ./build.sh --eval ./eval.py --results-file training.exhaustive.results sum.ml
echo exploration
aperf --build ./build.sh --eval ./eval.py --explore --results-file training.explore.results sum.ml
