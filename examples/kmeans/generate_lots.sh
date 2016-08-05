#!/usr/bin/env zsh

mkdir -p inputs

for i in {1..$1}; do
    python generate.py $2 > inputs/$i.in
done
