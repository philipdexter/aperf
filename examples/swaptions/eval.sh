#!/usr/bin/env zsh

sed -e 's/nan/0/' $1.out | ocaml hjm_securities_eval.ml
