#!/usr/bin/env python
import sys
answers = []
with open('answers.in') as f:
    answers = list(map(int, f.readlines()))

with open('sum.out') as f:
    results = list(map(int, f.readlines()))
    agg = 0
    if len(answers) != len(results):
        print(1)
        exit(0)
    for expected, received in zip(answers, results):
        agg += abs(expected - received)/expected
    agg_expected = sum(answers)
    print(agg/len(results))

