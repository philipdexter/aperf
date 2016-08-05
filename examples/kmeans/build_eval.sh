#!/usr/bin/env sh

ocamlfind ocamlopt -linkpkg -thread -package core,str point.ml eval.ml -o eval.native
