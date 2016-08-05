#!/usr/bin/env zsh

for i in {1..$1}; do
    python generate.py $2 $3 > input.$i
done

python gen_answers.py $(ls input.* | sort -t. -k2 -g) > answers.in
