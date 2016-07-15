#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ppperforate" @@ fun c->
  Ok [Pkg.bin "aperf/aperf"]

