#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "aperf" @@ fun c->
  Ok [Pkg.bin "aperf/aperf"]

