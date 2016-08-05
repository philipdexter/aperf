#!/usr/bin/env zsh

cp $1 tmp/kmeans.ml

cd tmp

ocamlfind ocamlopt -linkpkg -thread -package str -o $(basename $2) point.ml kmeans.ml main.ml
